open Datascript_types

type bindings = (string * query_result) list
type rule_call_key = string * string * query_result option list

module Make (Context : sig
  val empty_db : unit -> db
  val validate_rule_arities : query_rule list -> query_rule list
  val initial_query_context : db -> query -> query_arg list -> Query.query_callables * bindings list * query_rule list
  val eval_clauses :
    ?active_rules:rule_call_key list ->
    ?callables:Query.query_callables ->
    ?default_source:query_source ->
    db ->
    (string * query_source) list ->
    query_rule list ->
    bindings list ->
    query_clause list ->
    bindings list
  val eval_relation_rows :
    db ->
    (string * query_source) list ->
    query_rule list ->
    bindings list ->
    query_clause list ->
    (string list * query_result array list * bool) option
  val has_aggregates : find_spec list -> bool
  val aggregate_rows : ?callables:Query.query_callables -> db -> (string * query_source) list -> bindings list -> find_spec list -> query_result list list
  val aggregate_rows_with : ?callables:Query.query_callables -> db -> (string * query_source) list -> bindings list -> find_spec list -> string list -> query_result list list
  val non_aggregate_rows_with : db -> (string * query_source) list -> bindings list -> find_spec list -> string list -> query_result list list
  val collect_find_specs : db -> (string * query_source) list -> bindings -> find_spec list -> query_result list option
  val parse_query_string_with_pull_context : ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query
  val parse_query_return_string_with_pull_context : ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query_return * query
  val parse_query_return_map_string_with_pull_context : ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query_return * query_return_map option * query
  val compare_value : value -> value -> int
end) = struct
  open Context

  let ( let* ) = Option.bind

  let rule_names = Query.rule_names
  let resolve_dynamic_rule_clause = Query.resolve_dynamic_rule_clause
  let resolve_dynamic_rule = Query.resolve_dynamic_rule

  let query_rules_and_where query input_rules =
    let rules = validate_rule_arities (query.rules @ input_rules) in
    let names = rule_names rules in
    List.map (resolve_dynamic_rule names) rules, List.map (resolve_dynamic_rule_clause names) query.where

  let query_callables_empty (callables : Query.query_callables) =
    callables.callable_predicates = []
    && callables.callable_functions = []
    && callables.callable_aggregates = []
    && callables.callable_aliases = []

  let find_var_names = function
    | [] -> Some []
    | find ->
      let rec collect acc = function
        | [] -> Some (List.rev acc)
        | Find_var var :: rest -> collect (var :: acc) rest
        | (Find_pull _
          | Find_pull_form _
          | Find_pull_var _
          | Find_pull_source _
          | Find_pull_source_form _
          | Find_pull_source_var _
          | Find_aggregate _) :: _ ->
          None
      in
      collect [] find

  (* Typed row comparator: polymorphic [compare] on query_result rows
     pays the generic runtime dispatch (caml_compare/do_key) per node
     and dominated join query profiles. Ordering reproduces the
     polymorphic contract exactly: constructor declaration order across
     variants, then per-argument comparison. *)
  let rec compare_query_value left right =
    match left, right with
    | Nil, Nil -> 0
    | Int64 left, Int64 right -> Int64.compare left right
    | Float left, Float right -> compare left right
    | String left, String right -> compare left right
    | Symbol left, Symbol right -> compare left right
    | Bool left, Bool right -> compare left right
    | Keyword left, Keyword right -> compare left right
    | Uuid left, Uuid right -> compare left right
    | Instant left, Instant right -> Int64.compare left right
    | Regex left, Regex right -> compare left right
    | Ref left, Ref right -> compare left right
    | List left, List right -> compare_value_list left right
    | Vector left, Vector right -> compare_value_list left right
    | Tuple left, Tuple right ->
      let rec loop left right =
        match left, right with
        | [], [] -> 0
        | Some left :: ls, Some right :: rs ->
          let order = compare_query_value left right in
          if order <> 0 then order else loop ls rs
        | None :: ls, None :: rs -> loop ls rs
        | Some _ :: _, None :: _ -> 1
        | None :: _, Some _ :: _ -> -1
        | [], _ :: _ -> -1
        | _ :: _, [] -> 1
      in
      loop left right
    | Map _, Map _ | Set _, Set _ | TxRef, TxRef | Ref_to _, Ref_to _ ->
      compare left right
    | _ -> compare (value_rank left) (value_rank right)

  and compare_value_list left right =
    match left, right with
    | [], [] -> 0
    | left :: ls, right :: rs ->
      let order = compare_query_value left right in
      if order <> 0 then order else compare_value_list ls rs
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1

  and value_rank = function
    (* OCaml compare orders immediate constructors (no arguments)
       before block constructors; within each group the order follows
       declaration order. *)
    | Nil -> 0
    | TxRef -> 1
    | Int64 _ -> 2
    | Float _ -> 3
    | String _ -> 4
    | Symbol _ -> 5
    | Bool _ -> 6
    | Keyword _ -> 7
    | Uuid _ -> 8
    | Instant _ -> 9
    | Regex _ -> 10
    | Ref _ -> 11
    | List _ -> 12
    | Vector _ -> 13
    | Map _ -> 14
    | Set _ -> 15
    | Tuple _ -> 16
    | Ref_to _ -> 17

  let compare_query_result left right =
    match left, right with
    | Result_entity left, Result_entity right -> compare left right
    | Result_attr left, Result_attr right -> compare left right
    | Result_value left, Result_value right -> compare_query_value left right
    | Result_db _, Result_db _ | Result_pull _, Result_pull _ ->
      compare left right
    | _ ->
      let rank = function
        | Result_entity _ -> 0
        | Result_attr _ -> 1
        | Result_value _ -> 2
        | Result_db _ -> 3
        | Result_pull _ -> 4
      in
      compare (rank left) (rank right)

  let rec compare_rows left right =
    match left, right with
    | [], [] -> 0
    | left :: ls, right :: rs ->
      let order = compare_query_result left right in
      if order <> 0 then order else compare_rows ls rs
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1

  (* Typed hash for dedup tables — caml_hash walks the generic runtime
     representation and is markedly slower than hashing each leaf by
     constructor. Result_db/Result_pull never appear in result rows. *)
  let rec query_result_hash = function
    | Result_entity e -> Hashtbl.hash e * 5 + 1
    | Result_attr a -> Hashtbl.hash a * 31 + 2
    | Result_value v -> query_value_hash v * 31 + 3
    | Result_db _ -> 0
    | Result_pull _ -> 1
  and query_value_hash = function
    | Int64 i -> Hashtbl.hash i * 7 + 11
    | Float f -> Hashtbl.hash f * 7 + 12
    | String s | Symbol s | Uuid s | Regex s -> Hashtbl.hash s * 7 + 13
    | Keyword s -> Hashtbl.hash s * 7 + 14
    | Bool b -> Hashtbl.hash b * 7 + 15
    | Instant i -> Hashtbl.hash i * 7 + 16
    | Ref e -> Hashtbl.hash e * 7 + 17
    | List vs | Vector vs | Set vs ->
      List.fold_left (fun acc v -> acc * 7 + query_value_hash v) 18 vs
    | Tuple vs ->
      List.fold_left
        (fun acc v ->
          match v with
          | Some v -> acc * 7 + query_value_hash v
          | None -> acc * 7 + 1)
        19 vs
    | Map entries ->
      List.fold_left
        (fun acc (k, v) -> acc * 7 + query_value_hash k + query_value_hash v)
        20 entries
    | Nil | TxRef | Ref_to _ -> 0

  module Array_row_table = Hashtbl.Make (struct
    type t = query_result array

    let equal (a : t) (b : t) = a = b

    let hash row = Array.fold_left (fun acc v -> acc * 31 + query_result_hash v) 0 row
  end)

  module List_row_table = Hashtbl.Make (struct
    type t = query_result list

    let equal (a : t) (b : t) = a = b

    let hash row = List.fold_left (fun acc v -> acc * 31 + query_result_hash v) 0 row
  end)

  (* O(n) hash dedup preserving first-occurrence order. [q] results are
     sets, so row order is unspecified — sorting survivors would only add
     O(n log n) cost for no observable difference. *)
  let dedup rows =
    let seen = List_row_table.create (List.length rows) in
    List.fold_left
      (fun acc row ->
        if List_row_table.mem seen row then acc
        else (List_row_table.add seen row (); row :: acc))
      [] rows
    |> List.rev

  let dedup_rows_array rows =
    let seen = Array_row_table.create (List.length rows) in
    List.fold_left
      (fun acc row ->
        if Array_row_table.mem seen row then acc
        else (Array_row_table.add seen row (); row :: acc))
      [] rows
    |> List.rev

  let relation_rows_for_plain_find attrs (rows : query_result array list) unique_rows find =
    let* find_vars = find_var_names find in
    if find_vars = attrs then
      Some
        (if unique_rows then List.map Array.to_list rows
         else dedup_rows_array rows |> List.map Array.to_list)
    else
      let* indexes =
        find_vars
        |> List.fold_left
             (fun indexes var ->
               match indexes with
               | None -> None
               | Some indexes ->
                 (match List.find_index (( = ) var) attrs with
                  | Some index -> Some (index :: indexes)
                  | None -> None))
             (Some [])
        |> Option.map List.rev
      in
      let index_array = Array.of_list indexes in
      let project row = Array.map (Array.get row) index_array in
      (* unique_rows covers only unprojected rows: projection collapses
         columns, so distinct input rows can map onto the same output row —
         dedup must run regardless of the flag. *)
      let seen = Array_row_table.create (List.length rows) in
      Some
        (List.fold_left
           (fun acc row ->
             let projected = project row in
             if Array_row_table.mem seen projected then acc
             else (Array_row_table.add seen projected (); Array.to_list projected :: acc))
           [] rows
         |> List.rev)

  let find_spec_vars = function
    | Find_var var
    | Find_pull (var, _)
    | Find_pull_form (var, _)
    | Find_pull_source (_, var, _) ->
      [ var ]
    | Find_pull_source_form (_, var, _) -> [ var ]
    | Find_pull_var (var, pattern_var)
    | Find_pull_source_var (_, var, pattern_var) ->
      [ var; pattern_var ]
    | Find_aggregate _ -> []

  let relation_rows_for_find db sources attrs (rows : query_result array list) unique_rows find =
    match relation_rows_for_plain_find attrs rows unique_rows find with
    | Some rows -> Some rows
    | None ->
      let required_vars = find |> List.concat_map find_spec_vars |> List.sort_uniq compare in
      if required_vars <> [] && List.for_all (fun var -> List.mem var attrs) required_vars then
        rows
        |> List.filter_map (fun row ->
             collect_find_specs db sources (List.combine attrs (Array.to_list row)) find)
        |> dedup
        |> fun rows -> Some rows
      else
        None

  let dedupe_bindings_for_find bindings find =
    let vars = Query.grouping_vars_of_find find in
    match vars with
    | [] -> bindings
    | vars ->
      bindings
      |> List.filter_map (fun binding ->
        Query.collect_find_vars binding vars
        |> Option.map (fun key -> key, binding))
      |> fun pairs ->
        let seen = Hashtbl.create (List.length pairs) in
        List.fold_left
          (fun acc (key, binding) ->
            if Hashtbl.mem seen key then acc else (Hashtbl.add seen key (); binding :: acc))
          [] pairs
        |> List.rev
  
  let q_sources_raw ?(inputs = []) db sources query =
    let callables, input_bindings, input_rules = initial_query_context db query inputs in
    let rules, where =
      match query.rules, input_rules with
      | [], [] -> [], query.where
      | _ -> query_rules_and_where query input_rules
    in
    let has_aggregates = has_aggregates query.find in
    if
      (not has_aggregates)
      && query.with_vars = []
      && query_callables_empty callables
    then
      match eval_relation_rows db sources rules input_bindings where with
      | Some (attrs, rows, unique_rows) ->
        (match relation_rows_for_find db sources attrs rows unique_rows query.find with
         | Some rows -> rows
         | None ->
           let bindings = eval_clauses ~callables db sources rules input_bindings where in
           bindings
           |> fun bindings -> dedupe_bindings_for_find bindings query.find
           |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
           |> dedup)
      | None ->
        let bindings = eval_clauses ~callables db sources rules input_bindings where in
        bindings
        |> fun bindings -> dedupe_bindings_for_find bindings query.find
        |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
        |> dedup
    else (
      let bindings = eval_clauses ~callables db sources rules input_bindings where in
      if has_aggregates then
      if query.with_vars = [] then
        aggregate_rows ~callables db sources bindings query.find
      else
        aggregate_rows_with ~callables db sources bindings query.find query.with_vars
      else if query.with_vars <> [] then
      non_aggregate_rows_with db sources bindings query.find query.with_vars
      else
      bindings
      |> fun bindings -> dedupe_bindings_for_find bindings query.find
      |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
      |> dedup)
  
  let q_with_raw ?(inputs = []) db with_vars query =
    let callables, input_bindings, input_rules = initial_query_context db query inputs in
    let rules, where = query_rules_and_where query input_rules in
    let bindings = eval_clauses ~callables db [] rules input_bindings where in
    let with_vars = query.with_vars @ with_vars |> List.sort_uniq compare in
    if has_aggregates query.find then
      aggregate_rows_with ~callables db [] bindings query.find with_vars
    else
      non_aggregate_rows_with db [] bindings query.find with_vars

  let query_context : Query.context =
    { empty_db
    ; q_sources = q_sources_raw
    ; q_with = q_with_raw
    ; parse_query_string_with_pull_context
    ; parse_query_return_string_with_pull_context
    ; parse_query_return_map_string_with_pull_context
    ; compare_value
    }
  
end
