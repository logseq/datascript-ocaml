open Datascript_types

type bindings = (string * query_result) list
type rule_call_key = string * string * query_result option list

(** Which engine finished the last [q] relation path (for tests / diagnostics). *)
type query_exec_path =
  | Fused_execute
  | Relation_fallback
  | Binding_interpreter

let force_relation_fallback = ref false
let last_path = ref Relation_fallback

let last_query_exec_path () = !last_path

let with_force_relation_fallback f =
  let previous = !force_relation_fallback in
  force_relation_fallback := true;
  Fun.protect ~finally:(fun () -> force_relation_fallback := previous) f

(* Datalevin-style result cache: general, keyed by db epoch + physical query/inputs.
   Avoid structural hashing of [query] — where clauses may embed function values. *)
let result_cache_enabled =
  match Sys.getenv_opt "DATASCRIPT_QUERY_RESULT_CACHE" with
  | Some ("0" | "false" | "no" | "off") -> ref false
  | _ -> ref true

let query_debug_enabled =
  match Sys.getenv_opt "DATASCRIPT_QUERY_DEBUG" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | _ -> false

type result_cache_entry =
  { max_e : entity_id
  ; max_tx : int
  ; query : query
  ; inputs : query_arg list
  ; path : query_exec_path
  ; rows : query_result list list
  }

let result_cache_entries : result_cache_entry list ref = ref []
let result_cache_limit = 128
let last_cache_hit = ref false

let clear_query_result_cache () = result_cache_entries := []

let with_query_result_cache enabled f =
  let previous = !result_cache_enabled in
  result_cache_enabled := enabled;
  Fun.protect ~finally:(fun () -> result_cache_enabled := previous) f

let query_result_cache_enabled () = !result_cache_enabled
let last_query_cache_hit () = !last_cache_hit

let now_ms () = Platform.now_seconds () *. 1000.

let debug_log fmt =
  if query_debug_enabled then Printf.eprintf ("query-debug\t" ^^ fmt ^^ "\n%!")
  else Printf.ifprintf stderr fmt

let path_label = function
  | Fused_execute -> "fused"
  | Relation_fallback -> "relation"
  | Binding_interpreter -> "bindings"

let lookup_result_cache db query inputs =
  if (not !result_cache_enabled) || !force_relation_fallback then None
  else
    List.find_map
      (fun entry ->
        if
          entry.max_e = db.max_datom_e
          && entry.max_tx = db.max_tx
          && entry.query == query
          && entry.inputs == inputs
        then Some (entry.path, entry.rows)
        else None)
      !result_cache_entries

let store_result_cache db query inputs path rows =
  if !result_cache_enabled && not !force_relation_fallback then (
    let entry =
      { max_e = db.max_datom_e; max_tx = db.max_tx; query; inputs; path; rows }
    in
    let rest =
      List.filter
        (fun e ->
          not
            (e.query == query
             && e.inputs == inputs
             && e.max_e = entry.max_e
             && e.max_tx = entry.max_tx))
        !result_cache_entries
    in
    let rec take n = function
      | [] -> []
      | _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
    in
    result_cache_entries := take result_cache_limit (entry :: rest))

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
    (string list * query_result list list * bool) option
  val execute_plan :
    db ->
    (string * query_source) list ->
    query_rule list ->
    bindings list ->
    Query_plan.physical_plan ->
    (string list * query_result list list * bool) option
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

  let sort_uniq_presorted compare rows =
    let rec collect previous acc = function
      | [] -> Some (List.rev acc)
      | row :: rest ->
        (match previous with
         | None -> collect (Some row) (row :: acc) rest
         | Some previous ->
           let order = compare previous row in
           if order = 0 then
             collect (Some previous) acc rest
           else if order < 0 then
             collect (Some row) (row :: acc) rest
           else
             None)
    in
    match collect None [] rows with
    | Some rows -> rows
    | None -> List.sort_uniq compare rows

  let relation_rows_for_plain_find attrs rows unique_rows find =
    let* find_vars = find_var_names find in
    if find_vars = attrs then
      Some (if unique_rows then rows else sort_uniq_presorted compare rows)
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
      rows
      |> List.map (fun row -> indexes |> List.map (fun index -> List.nth row index))
      |> sort_uniq_presorted compare
      |> fun rows -> Some rows

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

  let relation_rows_for_find db sources attrs rows unique_rows find =
    match relation_rows_for_plain_find attrs rows unique_rows find with
    | Some rows -> Some rows
    | None ->
      let required_vars = find |> List.concat_map find_spec_vars |> List.sort_uniq compare in
      if required_vars <> [] && List.for_all (fun var -> List.mem var attrs) required_vars then
        rows
        |> List.filter_map (fun row -> collect_find_specs db sources (List.combine attrs row) find)
        |> List.sort_uniq compare
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
      |> List.sort_uniq (fun (left, _) (right, _) -> compare left right)
      |> List.map snd

  (* Do not Hashtbl-key by query_clause list: clauses may embed function
     values, and structural hashing/compare raises
     Invalid_argument("compare: functional value"). Physical equality on
     the reused where list (cached_query_string) is enough for the hot path. *)
  let last_plan_where : query_clause list ref = ref []
  let last_plan_max_e = ref (-1)
  let last_plan : Query_plan.physical_plan option ref = ref None

  let compile_plan max_datom_e where =
    if !last_plan_max_e = max_datom_e && !last_plan_where == where then
      !last_plan
    else (
      let plan = Query_plan.compile ~max_datom_e where in
      last_plan_where := where;
      last_plan_max_e := max_datom_e;
      last_plan := plan;
      plan)

  (* cached_query_string reuses the same find list object across calls. *)
  let last_find : find_spec list ref = ref []
  let last_find_vars : string list option ref = ref None

  let find_var_names_cached find =
    if !last_find == find then
      !last_find_vars
    else (
      last_find := find;
      let vars = find_var_names find in
      last_find_vars := vars;
      vars)

  let q_sources_raw ?(inputs = []) db sources query =
    let started = if query_debug_enabled then now_ms () else 0. in
    last_cache_hit := false;
    match lookup_result_cache db query inputs with
    | Some (path, rows) ->
      last_cache_hit := true;
      last_path := path;
      debug_log
        "cache=hit\tpath=%s\trows=%d\tms=%.4f"
        (path_label path)
        (List.length rows)
        (if query_debug_enabled then now_ms () -. started else 0.);
      rows
    | None ->
    let finish_relation_rows rules input_bindings where find =
      let plan_started = if query_debug_enabled then now_ms () else 0. in
      let try_planned_execute () =
        (* Prefer Datahike execute for single fused entity-group / ground scan.
           Multi-op Union and open scans still use relational fallback until
           probe-join / union execute matches those paths. *)
        if !force_relation_fallback || input_bindings <> [ [] ] then
          None
        else
          let plan =
            match rules with
            | [] -> compile_plan db.max_datom_e where
            | rules -> Query_plan.compile ~max_datom_e:db.max_datom_e ~rules where
          in
          match plan with
          | Some plan when Query_plan.plan_is_fused_execute plan -> (
            match plan.ops with
            | [ Query_plan.OpScan { clause; _ } ] -> (
              (* Only ground AVET-style scans are competitive on the execute path. *)
              match Query_plan.pattern_scan clause with
              | Some { entity = QVar _; attr = QAttr _; value = QValue _; tx = None; _ } ->
                execute_plan db sources [] input_bindings plan
              | _ -> None)
            | _ -> execute_plan db sources [] input_bindings plan)
          | _ -> None
      in
      let plan_ms = if query_debug_enabled then now_ms () -. plan_started else 0. in
      let exec_started = if query_debug_enabled then now_ms () else 0. in
      let relation_result =
        match try_planned_execute () with
        | Some result ->
          last_path := Fused_execute;
          Some result
        | None -> (
          match eval_relation_rows db sources rules input_bindings where with
          | Some result ->
            last_path := Relation_fallback;
            Some result
          | None ->
            last_path := Binding_interpreter;
            None)
      in
      let exec_ms = if query_debug_enabled then now_ms () -. exec_started else 0. in
      let rows =
        match relation_result with
        | Some (attrs, rows, unique_rows) -> (
          (* Hot path: find vars already match relation attrs (entity-group emit). *)
          match find_var_names_cached find with
          | Some find_vars when find_vars = attrs ->
            if unique_rows then rows else sort_uniq_presorted compare rows
          | _ ->
            (match relation_rows_for_find db sources attrs rows unique_rows find with
             | Some rows -> rows
             | None ->
               let bindings = eval_clauses db sources rules input_bindings where in
               bindings
               |> fun bindings -> dedupe_bindings_for_find bindings find
               |> List.filter_map (fun binding -> collect_find_specs db sources binding find)
               |> List.sort_uniq compare))
        | None ->
          let bindings = eval_clauses db sources rules input_bindings where in
          bindings
          |> fun bindings -> dedupe_bindings_for_find bindings find
          |> List.filter_map (fun binding -> collect_find_specs db sources binding find)
          |> List.sort_uniq compare
      in
      debug_log
        "cache=miss\tpath=%s\tplan-ms=%.4f\texec-ms=%.4f\trows=%d\ttotal-ms=%.4f"
        (path_label !last_path)
        plan_ms
        exec_ms
        (List.length rows)
        (if query_debug_enabled then now_ms () -. started else 0.);
      rows
    in
    let rows =
      if
        inputs = []
        && query.inputs = []
        && query.rules = []
        && query.with_vars = []
        && not (has_aggregates query.find)
      then
        finish_relation_rows [] [ [] ] query.where query.find
      else
        let callables, input_bindings, input_rules = initial_query_context db query inputs in
        let rules, where =
          match query.rules, input_rules with
          | [], [] -> [], query.where
          | _ -> query_rules_and_where query input_rules
        in
        if
          (not (has_aggregates query.find))
          && query.with_vars = []
          && query_callables_empty callables
        then
          finish_relation_rows rules input_bindings where query.find
        else (
          last_path := Binding_interpreter;
          let bindings = eval_clauses ~callables db sources rules input_bindings where in
          if has_aggregates query.find then
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
            |> List.sort_uniq compare)
    in
    store_result_cache db query inputs !last_path rows;
    rows
 
  let q_with_raw ?(inputs = []) db with_vars query =
    last_path := Binding_interpreter;
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
