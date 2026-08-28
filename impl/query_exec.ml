(** Datahike-aligned query execute layer: run compiled physical ops.

    Entity-group execution follows Datahike [execute-group-direct] /
    [execute-per-cursor-merge] / [execute-sorted-merge] semantics:
    drive from the planned scan slice, then per-entity lookup merges
    (AEVT binary search ≈ lookupGE), with foldable NOT as anti-merges
    that exclude on hit. Dense aligned-array gather is intentionally
    not used — that path diverged from Datahike and regressed benches. *)

open Datascript_types

[@@@ocaml.warning "-67"]

type bindings = (string * query_result) list

type relation =
  { attrs : string list
  ; rows : query_result list list
  ; unique_rows : bool
  }

module Make (Context : sig
  val query_evaluator_context : Query_eval.evaluator_context
  val query_source_context : db -> Query.source_context
  val cardinality_one : db -> attr -> bool
  val datoms_by_attr_value : db -> attr -> value -> datom list
  val entity_ids_by_attr_value : db -> attr -> value -> entity_id list option
  val entity_ids_array_by_attr_value : db -> attr -> value -> entity_id array option
  val query_attr_uses_avet : db -> attr -> bool
  val query_value_uses_avet : value -> bool
  val aevt_attr_array : db -> attr -> datom array option
  val aevt_duplicate_datoms : db -> attr -> datom list
  val find_entity_in_aevt_array : datom array -> entity_id -> datom option
end) = struct
  open Context

  let ( let* ) = Option.bind

  let unique_vars terms =
    terms
    |> List.filter_map (function QVar name -> Some name | _ -> None)
    |> List.fold_left (fun vars var -> if List.mem var vars then vars else var :: vars) []
    |> List.rev

  let row_value row index =
    let rec loop current = function
      | [] -> invalid_arg "relation row is missing a value"
      | value :: _ when current = index -> value
      | _ :: rest -> loop (current + 1) rest
    in
    loop 0 row

  let relation_attr_index attrs attr =
    match List.find_index (( = ) attr) attrs with
    | Some index -> index
    | None -> invalid_arg "relation attribute is missing from row"

  let hash_join left right =
    let common = List.filter (fun attr -> List.mem attr right.attrs) left.attrs in
    let right_only = List.filter (fun attr -> not (List.mem attr left.attrs)) right.attrs in
    let attrs = left.attrs @ right_only in
    if left.attrs = [] && left.rows = [ [] ] then
      { right with attrs }
    else if right.attrs = [] && right.rows = [ [] ] then
      { left with attrs }
    else if common = [] then
      { attrs
      ; rows =
          List.concat_map
            (fun left_row -> List.map (fun right_row -> left_row @ right_row) right.rows)
            left.rows
      ; unique_rows = false
      }
    else
      let right_common_indexes = List.map (fun attr -> attr, relation_attr_index right.attrs attr) common in
      let right_by_key =
        right.rows
        |> List.fold_left
             (fun table row ->
               let key =
                 right_common_indexes
                 |> List.map (fun (attr, index) -> attr, row_value row index)
               in
               Hashtbl.replace table key row;
               table)
             (Hashtbl.create (List.length right.rows))
      in
      let left_common_indexes = List.map (fun attr -> attr, relation_attr_index left.attrs attr) common in
      let right_only_indexes = List.map (relation_attr_index right.attrs) right_only in
      let rows =
        left.rows
        |> List.concat_map (fun left_row ->
          let key =
            left_common_indexes |> List.map (fun (attr, index) -> attr, row_value left_row index)
          in
          match Hashtbl.find_opt right_by_key key with
          | None -> []
          | Some right_row ->
            let extra = List.map (fun index -> row_value right_row index) right_only_indexes in
            [ left_row @ extra ])
      in
      { attrs; rows; unique_rows = left.unique_rows && right.unique_rows && rows <> [] }

  let anti_join left right =
    let join_attrs = List.filter (fun attr -> List.mem attr right.attrs) left.attrs in
    if join_attrs = [] then
      Some left
    else
      let indexes = List.map (fun attr -> attr, relation_attr_index left.attrs attr) join_attrs in
      let excluded =
        right.rows
        |> List.fold_left
             (fun table row ->
               let key = indexes |> List.map (fun (attr, index) -> attr, row_value row index) in
               Hashtbl.replace table key ();
               table)
             (Hashtbl.create (List.length right.rows))
      in
      let rows =
        left.rows
        |> List.filter (fun row ->
          let key = indexes |> List.map (fun (attr, index) -> attr, row_value row index) in
          not (Hashtbl.mem excluded key))
      in
      Some { left with rows; unique_rows = left.unique_rows && rows <> [] }

  let eval_comparison_predicate_clause = Query_eval.eval_comparison_predicate_clause query_evaluator_context

  let filter_comparison db relation predicate left_term right_term =
    let rows =
      relation.rows
      |> List.filter (fun row ->
        let binding = List.combine relation.attrs row in
        eval_comparison_predicate_clause db binding predicate left_term right_term <> [])
    in
    { relation with rows; unique_rows = false }

  let empty_relation = { attrs = []; rows = [ [] ]; unique_rows = true }

  let direct_attr attr = not (query_evaluator_context.is_reverse_ref attr)

  let unique_rows_flag source_db attrs e_var =
    (not source_db.history)
    && source_db.duplicate_datoms = []
    && List.mem e_var attrs

  let avet_ids_array source_db attr value =
    if query_value_uses_avet value && query_attr_uses_avet source_db attr then
      entity_ids_array_by_attr_value source_db attr value
    else
      None

  let value_matches term v =
    match term with
    | QValue expected -> query_evaluator_context.compare_value v expected = 0
    | QWildcard -> true
    | QVar _ -> true
    | _ -> false

  (* Datahike merge-op: positive lookup or anti-merge (NOT folded into group). *)
  type merge_op =
    | Pos of
        { attr : string
        ; value_term : query_term
        ; bind_var : string option
        ; arr : datom array
        }
    | Anti of
        { attr : string
        ; value_term : query_term
        ; (* Ground anti: excluded bitset (batched lookupGE). Non-ground: AEVT arr. *)
          excluded : bytes option
        ; arr : datom array option
        }

  let preload_aevt source_db attr =
    if not (direct_attr attr && cardinality_one source_db attr) then
      None
    else
      aevt_attr_array source_db attr

  let attrs_of_positive e_var (scan : Query_plan.l_scan) merges =
    (scan :: merges)
    |> List.concat_map (fun (s : Query_plan.l_scan) -> [ QVar e_var; s.attr; s.value ])
    |> unique_vars

  let parse_pos_merge source_db (scan : Query_plan.l_scan) =
    match scan.attr, scan.value with
    | QAttr attr, (QVar v as value_term) ->
      let* arr = preload_aevt source_db attr in
      Some (Pos { attr; value_term; bind_var = Some v; arr })
    | QAttr attr, ((QValue _ | QWildcard) as value_term) ->
      let* arr = preload_aevt source_db attr in
      Some (Pos { attr; value_term; bind_var = None; arr })
    | _ -> None

  let anti_excluded_bitset source_db attr value =
    let max_entity = source_db.max_datom_e + 1 in
    let excluded = Bytes.make max_entity '\000' in
    let mark e =
      if e >= 0 && e < max_entity then Bytes.unsafe_set excluded e '\001'
    in
    (match avet_ids_array source_db attr value with
     | Some ids ->
       for i = 0 to Array.length ids - 1 do
         mark ids.(i)
       done
     | None -> (
       match entity_ids_by_attr_value source_db attr value with
       | Some ids -> List.iter mark ids
       | None -> datoms_by_attr_value source_db attr value |> List.iter (fun d -> mark d.e)));
    excluded

  let parse_anti_merge source_db (scan : Query_plan.l_scan) =
    match scan.attr, scan.value with
    | QAttr attr, QValue value when direct_attr attr ->
      (* Batch ground anti into a bitset — same membership as per-eid lookupGE. *)
      Some (Anti { attr; value_term = QValue value; excluded = Some (anti_excluded_bitset source_db attr value); arr = None })
    | QAttr attr, value_term when direct_attr attr ->
      let* arr = aevt_attr_array source_db attr in
      Some (Anti { attr; value_term; excluded = None; arr = Some arr })
    | _ -> None

  (* Driving scan slice → eid + optional scan-bound value.
     Mirrors Datahike index slice iteration over the planned :scan-op. *)
  type drive_cell =
    { eid : entity_id
    ; scan_var : string option
    ; scan_value : query_result
    }

  let dummy_drive = { eid = 0; scan_var = None; scan_value = Result_entity 0 }

  let driving_cells source_db e_var (scan : Query_plan.l_scan) =
    match scan.entity, scan.attr, scan.value, scan.tx with
    | QVar ev, QAttr attr, QValue value, None when ev = e_var && direct_attr attr -> (
      match avet_ids_array source_db attr value with
      | Some ids ->
        Some (Array.init (Array.length ids) (fun i -> { eid = ids.(i); scan_var = None; scan_value = Result_entity 0 }))
      | None ->
        let datoms = datoms_by_attr_value source_db attr value in
        Some
          (Array.of_list
             (List.map (fun d -> { eid = d.e; scan_var = None; scan_value = Result_entity 0 }) datoms)))
    | QVar ev, QAttr attr, QVar v, None
      when ev = e_var && v <> e_var && direct_attr attr && cardinality_one source_db attr -> (
      match aevt_attr_array source_db attr with
      | None -> None
      | Some primary ->
        let n = Array.length primary in
        let duplicates = aevt_duplicate_datoms source_db attr in
        let total = n + List.length duplicates in
        let cells = Array.make total dummy_drive in
        for i = 0 to n - 1 do
          let d = primary.(i) in
          cells.(i) <-
            { eid = d.e
            ; scan_var = Some v
            ; scan_value = Query.result_of_ref (Query.result_of_datom_v d)
            }
        done;
        List.iteri
          (fun j d ->
            cells.(n + j) <-
              { eid = d.e
              ; scan_var = Some v
              ; scan_value = Query.result_of_ref (Query.result_of_datom_v d)
              })
          duplicates;
        Some cells)
    | QVar ev, QAttr attr, QWildcard, None
      when ev = e_var && direct_attr attr && cardinality_one source_db attr -> (
      match aevt_attr_array source_db attr with
      | None -> None
      | Some primary ->
        let duplicates = aevt_duplicate_datoms source_db attr in
        let cells =
          Array.append
            (Array.map (fun d -> { eid = d.e; scan_var = None; scan_value = Result_entity 0 }) primary)
            (Array.of_list
               (List.map (fun d -> { eid = d.e; scan_var = None; scan_value = Result_entity 0 }) duplicates))
        in
        Some cells)
    | _ -> None

  (* Advance AEVT pointer to eid (Datahike ForwardCursor seekGE / next). *)
  let seek_aevt arr ptr eid =
    let len = Array.length arr in
    let i = !ptr in
    if i < len && arr.(i).e = eid then (
      incr ptr;
      Some arr.(i))
    else
      let rec skip j =
        if j >= len then (
          ptr := len;
          None)
        else
          let e = arr.(j).e in
          if e < eid then skip (j + 1)
          else if e = eid then (
            ptr := j + 1;
            Some arr.(j))
          else (
            ptr := j;
            None)
      in
      skip i

  let dense_base arr =
    let len = Array.length arr in
    if len = 0 then None
    else
      let base = arr.(0).e in
      if arr.(len - 1).e = base + len - 1 then Some (base, len) else None

  let lookup_dense arr base len eid =
    let index = eid - base in
    if index >= 0 && index < len && arr.(index).e = eid then Some arr.(index) else None

  let rows_of_array_rev rows count =
    let rec loop i acc =
      if i < 0 then acc else loop (i - 1) (rows.(i) :: acc)
    in
    loop (count - 1) []

  (* q-not shaped: AEVT scan + ground anti-merge (Datahike anti during scan). *)
  let execute_scan_anti_ground source_db e_var attrs (scan : Query_plan.l_scan) anti_attr anti_value =
    match scan.entity, scan.attr, scan.value with
    | QVar ev, QAttr seed_attr, QVar v
      when ev = e_var && v <> e_var && direct_attr seed_attr && cardinality_one source_db seed_attr
           && ((attrs = [ e_var; v ]) || (attrs = [ v; e_var ])) ->
      let* seed_arr = aevt_attr_array source_db seed_attr in
      let excluded = anti_excluded_bitset source_db anti_attr anti_value in
      let max_entity = Bytes.length excluded in
      let rows = ref [] in
      let emit_e_v eid value =
        if eid >= 0 && eid < max_entity && Bytes.unsafe_get excluded eid = '\000' then
          rows :=
            (if attrs = [ e_var; v ] then [ Result_entity eid; value ]
             else [ value; Result_entity eid ])
            :: !rows
      in
      for i = Array.length seed_arr - 1 downto 0 do
        let d = seed_arr.(i) in
        emit_e_v d.e (Result_value d.v)
      done;
      List.iter (fun d -> emit_e_v d.e (Result_value d.v)) (aevt_duplicate_datoms source_db seed_attr);
      Some !rows
    | _ -> None

  (* q2 / q-5-merge: const AVET drive + dense/cursor merges (Datahike sorted-merge). *)
  let execute_const_drive_merges source_db e_var attrs (scan : Query_plan.l_scan) merges =
    match scan.entity, scan.attr, scan.value with
    | QVar ev, QAttr drive_attr, QValue drive_value when ev = e_var && direct_attr drive_attr ->
      let* ids =
        match avet_ids_array source_db drive_attr drive_value with
        | Some ids -> Some ids
        | None ->
          Some
            (datoms_by_attr_value source_db drive_attr drive_value
             |> List.map (fun d -> d.e)
             |> Array.of_list)
      in
      let* pos_ops =
        let rec collect acc = function
          | [] -> Some (List.rev acc)
          | m :: rest ->
            (match parse_pos_merge source_db m with
             | None -> None
             | Some op -> collect (op :: acc) rest)
        in
        collect [] merges
      in
      let drive_len = Array.length ids in
      (match pos_ops, attrs with
       (* q2: one value merge *)
       | [ Pos { bind_var = Some v; arr; _ } ], [ a; b ]
         when (a = e_var && b = v) || (a = v && b = e_var) ->
         let out = ref [] in
         (match dense_base arr with
          | Some (base, len) ->
            for i = drive_len - 1 downto 0 do
              let eid = ids.(i) in
              let idx = eid - base in
              if idx >= 0 && idx < len && arr.(idx).e = eid then
                let rv = Result_value arr.(idx).v in
                out :=
                  (if a = e_var then [ Result_entity eid; rv ] else [ rv; Result_entity eid ]) :: !out
            done
          | None ->
            let ptr = ref 0 in
            for i = 0 to drive_len - 1 do
              let eid = ids.(i) in
              match seek_aevt arr ptr eid with
              | None -> ()
              | Some d ->
                let rv = Result_value d.v in
                out :=
                  (if a = e_var then [ Result_entity eid; rv ] else [ rv; Result_entity eid ]) :: !out
            done;
            out := List.rev !out);
         Some !out
       (* Multi merges (value binds + optional ground verifies) — q3/q4/q-5-merge *)
       | pos_ops, _ ->
         let bind_vars =
           pos_ops
           |> List.filter_map (function Pos { bind_var; _ } -> bind_var | Anti _ -> None)
         in
         let expected_attrs = e_var :: bind_vars in
         if attrs <> expected_attrs then
           None
         else
           let n_pos = List.length pos_ops in
           let arrs =
             Array.of_list (List.map (function Pos { arr; _ } -> arr | Anti _ -> [||]) pos_ops)
           in
           let terms =
             Array.of_list
               (List.map (function Pos { value_term; _ } -> value_term | Anti _ -> QWildcard) pos_ops)
           in
           let binds =
             Array.of_list
               (List.map (function Pos { bind_var; _ } -> bind_var | Anti _ -> None) pos_ops)
           in
           let dense = Array.map dense_base arrs in
           if not (Array.for_all Option.is_some dense) then
             (* Cursor fallback for non-dense *)
             let pointers = Array.init n_pos (fun _ -> ref 0) in
             let rows = Array.make drive_len [] in
             let count = ref 0 in
             for i = 0 to drive_len - 1 do
               let eid = ids.(i) in
               let ok = ref true in
               let bound = ref [] in
               let mi = ref 0 in
               while !ok && !mi < n_pos do
                 match seek_aevt arrs.(!mi) pointers.(!mi) eid with
                 | None -> ok := false
                 | Some d when value_matches terms.(!mi) d.v ->
                   (match binds.(!mi) with
                    | Some v ->
                      bound := (v, Query.result_of_ref (Query.result_of_datom_v d)) :: !bound
                    | None -> ());
                   incr mi
                 | Some _ -> ok := false
               done;
               if !ok then (
                 let table = Hashtbl.create (List.length attrs) in
                 Hashtbl.add table e_var (Result_entity eid);
                 List.iter (fun (v, r) -> Hashtbl.add table v r) !bound;
                 rows.(!count) <- List.map (Hashtbl.find table) attrs;
                 incr count)
             done;
             Some (rows_of_array_rev rows !count)
           else
             let dense = Array.map Option.get dense in
             let n_bind = List.length bind_vars in
             let base0, len0 = dense.(0) in
             let aligned = Array.for_all (fun (b, l) -> b = base0 && l = len0) dense in
             let all_free_binds =
               Array.for_all
                 (function
                   | QVar _ -> true
                   | _ -> false)
                 terms
               && Array.for_all Option.is_some binds
             in
             if aligned && all_free_binds && n_bind = n_pos then (
               let out = ref [] in
               (match n_bind with
                | 4 ->
                  for i = drive_len - 1 downto 0 do
                    let eid = ids.(i) in
                    let idx = eid - base0 in
                    if idx >= 0 && idx < len0 then
                      out :=
                        [ Result_entity eid
                        ; Result_value arrs.(0).(idx).v
                        ; Result_value arrs.(1).(idx).v
                        ; Result_value arrs.(2).(idx).v
                        ; Result_value arrs.(3).(idx).v
                        ]
                        :: !out
                  done
                | 2 ->
                  for i = drive_len - 1 downto 0 do
                    let eid = ids.(i) in
                    let idx = eid - base0 in
                    if idx >= 0 && idx < len0 then
                      out :=
                        [ Result_entity eid
                        ; Result_value arrs.(0).(idx).v
                        ; Result_value arrs.(1).(idx).v
                        ]
                        :: !out
                  done
                | 1 ->
                  for i = drive_len - 1 downto 0 do
                    let eid = ids.(i) in
                    let idx = eid - base0 in
                    if idx >= 0 && idx < len0 then
                      out := [ Result_entity eid; Result_value arrs.(0).(idx).v ] :: !out
                  done
                | _ ->
                  for i = drive_len - 1 downto 0 do
                    let eid = ids.(i) in
                    let idx = eid - base0 in
                    if idx >= 0 && idx < len0 then
                      let row = Array.make (n_bind + 1) (Result_entity eid) in
                      row.(0) <- Result_entity eid;
                      for j = 0 to n_bind - 1 do
                        row.(j + 1) <- Result_value arrs.(j).(idx).v
                      done;
                      out := Array.to_list row :: !out
                  done);
               Some !out)
             else
               (* Per-attr dense or mixed ground verifies *)
               let out = ref [] in
               for i = drive_len - 1 downto 0 do
                 let eid = ids.(i) in
                 let ok = ref true in
                 let vals = Array.make n_bind (Result_value (Int 0)) in
                 let vi = ref 0 in
                 let mi = ref 0 in
                 while !ok && !mi < n_pos do
                   let base, len = dense.(!mi) in
                   let idx = eid - base in
                   if idx < 0 || idx >= len || arrs.(!mi).(idx).e <> eid then ok := false
                   else
                     let d = arrs.(!mi).(idx) in
                     if not (value_matches terms.(!mi) d.v) then ok := false
                     else (
                       (match binds.(!mi) with
                        | Some _ ->
                          vals.(!vi) <- Result_value d.v;
                          incr vi
                        | None -> ());
                       incr mi)
                 done;
                 if !ok then (
                   let row = Array.make (n_bind + 1) (Result_entity eid) in
                   row.(0) <- Result_entity eid;
                   for j = 0 to n_bind - 1 do
                     row.(j + 1) <- vals.(j)
                   done;
                   out := Array.to_list row :: !out)
               done;
               Some !out)
    | _ -> None

  (* Datahike execute-sorted-merge / per-cursor-merge for card-one attrs. *)
  let execute_lookup_merge source_db e_var attrs (scan : Query_plan.l_scan) merges anti_scans =
    match merges, anti_scans with
    | [], [ { Query_plan.attr = QAttr anti_attr; value = QValue anti_value; _ } ] ->
      execute_scan_anti_ground source_db e_var attrs scan anti_attr anti_value
    | [], [ _ ] -> None
    | merges, [] -> execute_const_drive_merges source_db e_var attrs scan merges
    | _ ->
      (* Mixed positive + anti: drive + cursor merges + anti bitset/lookup. *)
      let* drive = driving_cells source_db e_var scan in
      let* pos_ops =
        let rec collect acc = function
          | [] -> Some (List.rev acc)
          | m :: rest ->
            (match parse_pos_merge source_db m with
             | None -> None
             | Some op -> collect (op :: acc) rest)
        in
        collect [] merges
      in
      let* anti_ops =
        let rec collect acc = function
          | [] -> Some (List.rev acc)
          | m :: rest ->
            (match parse_anti_merge source_db m with
             | None -> None
             | Some op -> collect (op :: acc) rest)
        in
        collect [] anti_scans
      in
      let pos_arr = Array.of_list pos_ops in
      let n_pos = Array.length pos_arr in
      let pointers = Array.init n_pos (fun _ -> ref 0) in
      let dense =
        Array.map
          (function
            | Pos { arr; _ } -> dense_base arr
            | Anti _ -> None)
          pos_arr
      in
      let anti_arr = Array.of_list anti_ops in
      let n_anti = Array.length anti_arr in
      let drive_len = Array.length drive in
      let rows = Array.make drive_len [] in
      let count = ref 0 in
      let bind_buf = Array.make (List.length attrs) (Result_entity 0) in
      let attr_index =
        let tbl = Hashtbl.create (List.length attrs) in
        List.iteri (fun i name -> Hashtbl.add tbl name i) attrs;
        tbl
      in
      let set_bind var value =
        match Hashtbl.find_opt attr_index var with
        | Some i -> bind_buf.(i) <- value
        | None -> ()
      in
      for i = 0 to drive_len - 1 do
        let cell = drive.(i) in
        let eid = cell.eid in
        set_bind e_var (Result_entity eid);
        (match cell.scan_var with
         | Some v -> set_bind v cell.scan_value
         | None -> ());
        let ok = ref true in
        let mi = ref 0 in
        while !ok && !mi < n_pos do
          match pos_arr.(!mi) with
          | Pos { bind_var; value_term; arr; _ } -> (
            let found =
              match dense.(!mi) with
              | Some (base, len) -> lookup_dense arr base len eid
              | None -> seek_aevt arr pointers.(!mi) eid
            in
            match found with
            | None -> ok := false
            | Some d when value_matches value_term d.v ->
              (match bind_var with
               | Some v -> set_bind v (Query.result_of_ref (Query.result_of_datom_v d))
               | None -> ());
              incr mi
            | Some _ -> ok := false)
          | Anti _ -> incr mi
        done;
        let ai = ref 0 in
        while !ok && !ai < n_anti do
          (match anti_arr.(!ai) with
           | Anti { excluded = Some excluded; _ } ->
             let max_entity = Bytes.length excluded in
             if eid >= 0 && eid < max_entity && Bytes.unsafe_get excluded eid = '\001' then
               ok := false
           | Anti { excluded = None; arr = Some arr; value_term; _ } -> (
             match find_entity_in_aevt_array arr eid with
             | Some d when value_matches value_term d.v -> ok := false
             | _ -> ())
           | Anti _ | Pos _ -> ());
          incr ai
        done;
        if !ok then (
          rows.(!count) <- Array.to_list bind_buf;
          incr count)
      done;
      Some (rows_of_array_rev rows !count)

  let execute_entity_group _db source (group : Query_plan.entity_group) =
    match source with
    | Db_source source_db -> (
      (* Predicates attached to the group: defer to relational fallback which
         already has AVET range pushdown (Datahike scan-bound path). *)
      if group.filters <> [] then
        None
      else
        let e_var = group.entity_var in
        let (scan : Query_plan.l_scan) = group.scan in
        match scan.entity with
        | QVar ev when ev = e_var ->
          let attrs = attrs_of_positive e_var scan group.merges in
          (match execute_lookup_merge source_db e_var attrs scan group.merges group.anti_scans with
           | None -> None
           | Some rows ->
             Some { attrs; rows; unique_rows = unique_rows_flag source_db attrs e_var })
        | _ -> None)
    | _ -> None

  let execute_scan db source (scan : Query_plan.l_scan) =
    match source with
    | Db_source source_db -> (
      (* Datahike :scan-only / AVET ground pattern (q1). *)
      match scan.entity, scan.attr, scan.value, scan.tx with
      | QVar e_var, QAttr attr, QValue value, None when direct_attr attr -> (
        match entity_ids_by_attr_value source_db attr value with
        | Some entity_ids ->
          Some
            { attrs = [ e_var ]
            ; rows = List.map (fun e -> [ Result_entity e ]) entity_ids
            ; unique_rows = unique_rows_flag source_db [ e_var ] e_var
            }
        | None ->
          let rows =
            datoms_by_attr_value source_db attr value
            |> List.map (fun datom -> [ Result_entity datom.e ])
          in
          Some { attrs = [ e_var ]; rows; unique_rows = false })
      | _ ->
        let terms =
          match scan.tx with
          | None -> [ scan.entity; scan.attr; scan.value ]
          | Some tx -> [ scan.entity; scan.attr; scan.value; tx ]
        in
        let attrs = unique_vars terms in
        let source_context = query_source_context db in
        let datoms =
          match terms with
          | [ e_term; a_term; v_term ] -> source_context.pattern_datoms source_db e_term a_term v_term None
          | [ e_term; a_term; v_term; tx_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term (Some tx_term)
          | _ -> invalid_arg "scan expects 3 or 4 pattern terms"
        in
        let slots =
          attrs
          |> List.map (fun attr ->
            let rec find index = function
              | [] -> invalid_arg "scan variable missing from pattern"
              | QVar var :: _ when var = attr -> index
              | _ :: rest -> find (index + 1) rest
            in
            find 0 terms)
        in
        let build_row datom =
          slots
          |> List.map (fun index ->
            match index with
            | 0 -> Query.result_of_datom_e datom
            | 1 -> Query.result_of_datom_a datom
            | 2 -> Query.result_of_ref (Query.result_of_datom_v datom)
            | 3 -> Query.result_of_datom_tx datom
            | _ -> invalid_arg "invalid scan slot")
        in
        let rows =
          datoms
          |> Seq.fold_left (fun acc datom -> build_row datom :: acc) []
          |> List.rev
        in
        Some { attrs; rows; unique_rows = false })
    | _ -> None

  let rec execute_plan db sources default_source bindings plan =
    let rec apply relation = function
      | [] -> Some relation
      | Query_plan.OpEntityGroup group :: rest -> (
        match execute_entity_group db default_source group with
        | None -> None
        | Some next -> apply (hash_join relation next) rest)
      | Query_plan.OpScan { clause; source = op_source; _ } :: rest -> (
        let source =
          match op_source with
          | Some name -> Query.source db sources name
          | None -> default_source
        in
        match Query_plan.pattern_scan clause with
        | None -> None
        | Some scan -> (
          match execute_scan db source scan with
          | None -> None
          | Some next -> apply (hash_join relation next) rest))
      | Query_plan.OpFilter clause :: rest -> (
        match clause with
        | ComparisonPredicate (predicate, left_term, right_term) ->
          apply (filter_comparison db relation predicate left_term right_term) rest
        | _ -> None)
      | Query_plan.OpUnion { join_vars; branches } :: rest -> (
        let branch_relations =
          branches
          |> List.filter_map (fun branch -> execute_plan db sources default_source bindings branch)
        in
        if List.length branch_relations <> List.length branches then
          None
        else
          let* merged =
            match branch_relations with
            | [] -> Some empty_relation
            | first :: others ->
              Some
                (List.fold_left
                   (fun acc branch ->
                     match join_vars with
                     | None -> union_relations acc branch
                     | Some vars -> union_relations (project_relation vars acc) (project_relation vars branch))
                   first
                   others)
          in
          apply (hash_join relation merged) rest)
      | Query_plan.OpAntiJoin { join_vars; excluded } :: rest -> (
        let* excluded_relation = execute_plan db sources default_source bindings excluded in
        let filtered =
          match join_vars with
          | None -> relation
          | Some vars -> project_relation vars relation
        in
        let* joined = anti_join filtered excluded_relation in
        apply joined rest)
      | Query_plan.OpPassthrough _ :: _ -> None
    in
    apply empty_relation plan.ops

  and union_relations left right =
    let attrs = left.attrs @ List.filter (fun attr -> not (List.mem attr left.attrs)) right.attrs in
    let rows = left.rows @ right.rows |> List.sort_uniq compare in
    { attrs; rows; unique_rows = false }

  and project_relation vars relation =
    let indexes = vars |> List.map (relation_attr_index relation.attrs) in
    let attrs = vars in
    let rows =
      relation.rows
      |> List.filter_map (fun row ->
        try Some (indexes |> List.map (fun index -> row_value row index)) with _ -> None)
      |> List.sort_uniq compare
    in
    { attrs; rows; unique_rows = false }

  let run db sources rules bindings plan =
    if rules <> [] || bindings <> [ [] ] then
      None
    else
      let default_source = Query.source db sources "$" in
      execute_plan db sources default_source bindings plan
end
