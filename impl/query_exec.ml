(** Datahike-aligned query execute layer: run compiled physical ops. *)

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

  let classify_patterns e_var scans =
    scans
    |> List.fold_left
         (fun (value_vars, constants, required) (_, attr, value_term) ->
           match value_term with
           | QVar value_var when value_var <> e_var ->
             ((value_var, attr) :: value_vars, constants, required)
           | QValue value -> (value_vars, (attr, value) :: constants, required)
           | QWildcard -> (value_vars, constants, attr :: required)
           | QVar _ | QEntity _ | QAttr _ | QIdent _ | QLookupRef _ | QSource _ ->
             (value_vars, constants, required))
         ([], [], [])

  let attrs_of_scans e_var scans =
    scans
    |> List.concat_map (fun (_, attr, value_term) -> [ QVar e_var; QAttr attr; value_term ])
    |> unique_vars

  let attr_name = function QAttr name -> name | _ -> ""

  let scans_of_group (group : Query_plan.entity_group) =
    List.map
      (fun (scan : Query_plan.l_scan) ->
        match scan.entity with
        | QVar e_var -> e_var, attr_name scan.attr, scan.value
        | _ -> "", attr_name scan.attr, scan.value)
      (group.scan :: group.merges)

  let anti_patterns_of_group (group : Query_plan.entity_group) =
    List.map
      (fun (anti : Query_plan.l_scan) -> attr_name anti.attr, anti.value)
      group.anti_scans

  let avet_ids_array source_db attr value =
    if query_value_uses_avet value && query_attr_uses_avet source_db attr then
      entity_ids_array_by_attr_value source_db attr value
    else
      None

  let arrays_aligned const_arr attr_arrays =
    let const_len = Array.length const_arr in
    if const_len = 0 then
      false
    else if not (Array.for_all (fun arr -> Array.length arr = const_len) attr_arrays) then
      false
    else
      let mid = const_len / 2 in
      let check i =
        let e = const_arr.(i).e in
        Array.for_all (fun arr -> arr.(i).e = e) attr_arrays
      in
      check 0 && check mid && check (const_len - 1)

  let dense_range const_arr attr_arrays =
    let const_len = Array.length const_arr in
    let base_e = const_arr.(0).e in
    const_arr.(const_len - 1).e = base_e + const_len - 1
    && Array.for_all (fun arr -> arr.(0).e = base_e && arr.(const_len - 1).e = base_e + const_len - 1) attr_arrays

  let build_value_row e attr_arrays index =
    let rec vals a acc =
      if a < 0 then Result_entity e :: acc
      else vals (a - 1) (Result_value attr_arrays.(a).(index).v :: acc)
    in
    vals (Array.length attr_arrays - 1) []

  let gather_const_value_rows source_db e_var attrs const_attr const_value value_vars =
    let value_vars = List.rev value_vars in
    if
      value_vars = []
      || not (direct_attr const_attr)
      || not (List.for_all (fun (_, attr) -> direct_attr attr && cardinality_one source_db attr) value_vars)
    then
      None
    else
      let* const_arr = aevt_attr_array source_db const_attr in
      let value_attr_arrays =
        value_vars
        |> List.map (fun (value_var, attr) ->
          match aevt_attr_array source_db attr with
          | None -> None
          | Some arr -> Some (value_var, arr))
      in
      if List.exists Option.is_none value_attr_arrays then
        None
      else
        let value_attrs = value_attr_arrays |> List.map Option.get |> Array.of_list in
        let attr_arrays = Array.map (fun (_, arr) -> arr) value_attrs in
        if not (arrays_aligned const_arr attr_arrays) then
          None
        else
          let const_len = Array.length const_arr in
          let base_e = const_arr.(0).e in
          if not (dense_range const_arr attr_arrays) then
            None
          else
            let expected = e_var :: (value_attrs |> Array.to_list |> List.map fst) in
            if attrs <> expected then
              None
            else
              let rows = ref [] in
              (match avet_ids_array source_db const_attr const_value with
               | Some ids ->
                 for i = Array.length ids - 1 downto 0 do
                   let e = ids.(i) in
                   let index = e - base_e in
                   if index >= 0 && index < const_len then
                     rows := build_value_row e attr_arrays index :: !rows
                 done
               | None ->
                 for i = const_len - 1 downto 0 do
                   if query_evaluator_context.compare_value const_arr.(i).v const_value = 0 then
                     rows := build_value_row const_arr.(i).e attr_arrays i :: !rows
                 done);
              Some !rows

  let intersect_entity_ids id_lists =
    let rec intersect_sorted left right =
      match left, right with
      | [], _ | _, [] -> []
      | x :: xs, y :: ys ->
        if x = y then x :: intersect_sorted xs ys
        else if x < y then intersect_sorted xs right
        else intersect_sorted left ys
    in
    match List.sort (fun left right -> compare (List.length left) (List.length right)) id_lists with
    | [] -> []
    | smallest :: rest -> List.fold_left intersect_sorted smallest rest

  let entity_ids_for_constant source_db attr value =
    match entity_ids_by_attr_value source_db attr value with
    | Some entity_ids -> entity_ids
    | None -> datoms_by_attr_value source_db attr value |> List.map (fun datom -> datom.e)

  let gather_multi_constant_value_rows source_db e_var attrs constants value_vars =
    if
      constants = []
      || value_vars = []
      || not
           (List.for_all
              (fun (_, attr) -> direct_attr attr && cardinality_one source_db attr)
              value_vars)
    then
      None
    else
      let entity_sets = List.map (fun (attr, value) -> entity_ids_for_constant source_db attr value) constants in
      if List.exists (fun ids -> ids = []) entity_sets then
        Some []
      else
        let allowed = intersect_entity_ids entity_sets in
        if allowed = [] then
          Some []
        else
          match constants, value_vars with
          | [ (const_attr, const_value) ], _ ->
            gather_const_value_rows source_db e_var attrs const_attr const_value value_vars
          | _ :: _, value_vars -> (
            let allowed_set =
              let bytes = Bytes.make (source_db.max_datom_e + 1) '\000' in
              List.iter (fun e -> if e >= 0 && e < Bytes.length bytes then Bytes.set bytes e '\001') allowed;
              bytes
            in
            let filter_rows rows =
              List.filter
                (fun row ->
                  match row with
                  | Result_entity e :: _ -> e >= 0 && e < Bytes.length allowed_set && Bytes.get allowed_set e = '\001'
                  | _ -> false)
                rows
            in
            let (const_attr, const_value) = List.hd constants in
            match gather_const_value_rows source_db e_var attrs const_attr const_value value_vars with
            | None -> None
            | Some rows -> Some (filter_rows rows))
          | _ -> None

  let gather_not_rows source_db e_var attrs seed_attr value_var clause_attr clause_value =
    if not (direct_attr seed_attr && direct_attr clause_attr) then
      None
    else
      let* seed_arr = aevt_attr_array source_db seed_attr in
      let max_entity = source_db.max_datom_e + 1 in
      let excluded = Bytes.make max_entity '\000' in
      let mark_excluded entity_id =
        if entity_id >= 0 && entity_id < max_entity then Bytes.unsafe_set excluded entity_id '\001'
      in
      (match entity_ids_by_attr_value source_db clause_attr clause_value with
       | Some entity_ids -> List.iter mark_excluded entity_ids
       | None -> datoms_by_attr_value source_db clause_attr clause_value |> List.iter (fun datom -> mark_excluded datom.e));
      let rows = ref [] in
      let emit datom =
        if datom.e >= 0 && datom.e < max_entity && Bytes.unsafe_get excluded datom.e = '\000' then
          match attrs with
          | [ entity_attr; value_attr ] when entity_attr = e_var && value_attr = value_var ->
            rows := [ Result_entity datom.e; Query.result_of_datom_v datom ] :: !rows
          | [ value_attr; entity_attr ] when entity_attr = e_var && value_attr = value_var ->
            rows := [ Query.result_of_datom_v datom; Result_entity datom.e ] :: !rows
          | _ -> ()
      in
      for i = Array.length seed_arr - 1 downto 0 do
        emit seed_arr.(i)
      done;
      List.iter emit (aevt_duplicate_datoms source_db seed_attr);
      Some !rows

  let execute_entity_group db source (group : Query_plan.entity_group) =
    match source with
    | Db_source source_db ->
      let scans = scans_of_group group in
      let e_var = group.entity_var in
      if not (List.for_all (fun (candidate, _, _) -> candidate = e_var) scans) then
        None
      else
        let attrs = attrs_of_scans e_var scans
        in
        let value_var_patterns, constant_patterns, required_patterns =
          classify_patterns e_var scans
        in
        let duplicate_value_var =
          let seen = Hashtbl.create (List.length value_var_patterns) in
          List.exists
            (fun (value_var, _) ->
              if Hashtbl.mem seen value_var then true
              else (
                Hashtbl.add seen value_var ();
                false ))
            value_var_patterns
        in
        if duplicate_value_var || required_patterns <> [] then
          None
        else
          let anti = anti_patterns_of_group group in
          (match value_var_patterns, constant_patterns, anti with
           | [ (value_var, seed_attr) ], [], [ (clause_attr, QValue clause_value) ] ->
             gather_not_rows source_db e_var attrs seed_attr value_var clause_attr clause_value
           | value_vars, constants, [] when value_vars <> [] && constants <> [] -> (
             match constants with
             | [ (const_attr, const_value) ] ->
               gather_const_value_rows source_db e_var attrs const_attr const_value value_vars
             | _ ->
               gather_multi_constant_value_rows source_db e_var attrs constants value_vars)
           | [], [ (const_attr, const_value) ], [] -> (
             match entity_ids_by_attr_value source_db const_attr const_value with
             | Some entity_ids -> Some (List.map (fun e -> [ Result_entity e ]) entity_ids)
             | None ->
               Some
                 (datoms_by_attr_value source_db const_attr const_value
                  |> List.map (fun datom -> [ Result_entity datom.e ])))
           | _ -> None)
          |> Option.map (fun rows ->
            let relation = { attrs; rows; unique_rows = unique_rows_flag source_db attrs e_var } in
            List.fold_left
              (fun relation clause ->
                match clause with
                | ComparisonPredicate (predicate, left_term, right_term) ->
                  filter_comparison db relation predicate left_term right_term
                | _ -> relation)
              relation
              group.filters)
    | _ -> None

  let execute_scan db source (scan : Query_plan.l_scan) =
    match source with
    | Db_source source_db ->
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
        | [ e_term; a_term; v_term; tx_term ] -> source_context.pattern_datoms source_db e_term a_term v_term (Some tx_term)
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
      Some { attrs; rows; unique_rows = false }
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
