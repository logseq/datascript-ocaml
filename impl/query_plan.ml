(** Logical query plan IR and cost-based clause ordering (Phase 1–2 foundation). *)

open Datascript_types

type index_choice =
  | Prefer_eavt
  | Prefer_aevt
  | Prefer_avet

type pattern_access =
  { entity : query_term
  ; attr : query_term
  ; value : query_term
  ; tx : query_term option
  ; index : index_choice
  ; estimated_rows : int
  }

type logical_node =
  | Scan of pattern_access
  | RangeScan of pattern_access * comparison_predicate
  | MergeScan of pattern_access list
  | HashJoin of logical_node * logical_node
  | Filter of logical_node * query_clause
  | AntiJoin of logical_node * logical_node
  | Union of logical_node list
  | RuleExpand of string * query_term list * logical_node
  | Unsupported of query_clause

type plan =
  { nodes : logical_node list
  ; ordered_where : query_clause list
  }

let term_is_ground = function
  | QEntity _ | QIdent _ | QLookupRef _ | QAttr _ | QValue _ -> true
  | QVar _ | QSource _ | QWildcard -> false

let choose_index e_term a_term v_term =
  match term_is_ground e_term, term_is_ground a_term, term_is_ground v_term with
  | true, _, _ -> Prefer_eavt
  | false, true, true -> Prefer_avet
  | false, true, false -> Prefer_aevt
  | _ -> Prefer_eavt

let estimate_pattern_cost ?(max_datom_e = 1_000_000) e_term a_term v_term =
  let max_e = max 1 max_datom_e in
  match term_is_ground e_term, term_is_ground a_term, term_is_ground v_term with
  | true, _, _ -> 1
  | false, true, true -> 4
  | false, true, false -> max_e / 8
  | false, false, true -> max_e / 16
  | false, false, false -> max_e

let pattern_access_of_terms ~max_datom_e e_term a_term v_term tx_term =
  let index = choose_index e_term a_term v_term in
  let estimated_rows = estimate_pattern_cost ~max_datom_e e_term a_term v_term in
  { entity = e_term; attr = a_term; value = v_term; tx = tx_term; index; estimated_rows }

let same_entity_var left right =
  match left.entity, right.entity with
  | QVar a, QVar b -> a = b
  | QEntity a, QEntity b -> a = b
  | _ -> false

let rec collapse_merge_scans = function
  | [] -> []
  | Scan first :: rest ->
    let rec take_group acc = function
      | Scan next :: more when same_entity_var first next -> take_group (next :: acc) more
      | more -> List.rev acc, more
    in
    let group, rest = take_group [ first ] rest in
    (match group with
     | [ single ] -> Scan single :: collapse_merge_scans rest
     | many -> MergeScan many :: collapse_merge_scans rest)
  | node :: rest -> node :: collapse_merge_scans rest

let analyze_clause ~max_datom_e = function
  | Pattern (e, a, v) -> Some (Scan (pattern_access_of_terms ~max_datom_e e a v None))
  | PatternTx (e, a, v, tx) -> Some (Scan (pattern_access_of_terms ~max_datom_e e a v (Some tx)))
  | PatternTxOp (e, a, v, tx, _) -> Some (Scan (pattern_access_of_terms ~max_datom_e e a v (Some tx)))
  | Not [ Pattern (e, a, v) ] as outer ->
    let excluded = Scan (pattern_access_of_terms ~max_datom_e e a v None) in
    Some (AntiJoin (Unsupported outer, excluded))
  | NotJoin (_, [ Pattern (e, a, v) ]) as outer ->
    let excluded = Scan (pattern_access_of_terms ~max_datom_e e a v None) in
    Some (AntiJoin (Unsupported outer, excluded))
  | Or branches when List.for_all (function [ Pattern _ ] -> true | _ -> false) branches ->
    let nodes =
      List.filter_map
        (function
          | [ Pattern (e, a, v) ] -> Some (Scan (pattern_access_of_terms ~max_datom_e e a v None))
          | _ -> None)
        branches
    in
    Some (Union nodes)
  | OrJoin (_, branches) when List.for_all (function [ Pattern _ ] -> true | _ -> false) branches ->
    let nodes =
      List.filter_map
        (function
          | [ Pattern (e, a, v) ] -> Some (Scan (pattern_access_of_terms ~max_datom_e e a v None))
          | _ -> None)
        branches
    in
    Some (Union nodes)
  | Rule (name, terms) as clause ->
    Some (RuleExpand (name, terms, Unsupported clause))
  | clause -> Some (Unsupported clause)

let clause_sort_key ~max_datom_e clause =
  match clause with
  | Pattern (e, a, v) | PatternTx (e, a, v, _) | PatternTxOp (e, a, v, _, _) ->
    estimate_pattern_cost ~max_datom_e e a v
  | Not _ | NotJoin _ -> 1_000_000
  | Or _ | OrJoin _ | OrJoinRequired _ -> 900_000
  | Rule _ | SourceRule _ -> 800_000
  | ComparisonPredicate _ | EqualityPredicate _ -> 50
  | _ -> 500_000

let is_pattern_clause = function
  | Pattern _ | PatternTx _ | PatternTxOp _ -> true
  | _ -> false

(** Stable cost-order for leading pattern runs; leave non-pattern anchors in place. *)
let order_where_clauses ?(max_datom_e = 1_000_000) clauses =
  let rec reorder acc = function
    | [] -> List.rev acc
    | clause :: rest when is_pattern_clause clause ->
      let rec take_patterns collected = function
        | next :: more when is_pattern_clause next -> take_patterns (next :: collected) more
        | more -> List.rev collected, more
      in
      let patterns, rest = take_patterns [ clause ] rest in
      let sorted =
        patterns
        |> List.mapi (fun i c -> clause_sort_key ~max_datom_e c, i, c)
        |> List.sort (fun (c1, i1, _) (c2, i2, _) ->
          let cmp = compare c1 c2 in
          if cmp <> 0 then cmp else compare i1 i2)
        |> List.map (fun (_, _, c) -> c)
      in
      reorder (List.rev_append sorted acc) rest
    | clause :: rest -> reorder (clause :: acc) rest
  in
  reorder [] clauses

let left_deep_join = function
  | [] -> None
  | first :: rest ->
    Some (List.fold_left (fun left right -> HashJoin (left, right)) first rest)

let analyze ?(max_datom_e = 1_000_000) query =
  if query.with_vars <> [] then None
  else
    let ordered_where = order_where_clauses ~max_datom_e query.where in
    let nodes_opt =
      ordered_where
      |> List.fold_left
           (fun acc clause ->
             match acc with
             | None -> None
             | Some nodes ->
               (match analyze_clause ~max_datom_e clause with
                | None -> None
                | Some node -> Some (node :: nodes)))
           (Some [])
    in
    match nodes_opt with
    | None -> None
    | Some rev_nodes ->
      let nodes = collapse_merge_scans (List.rev rev_nodes) in
      let _ = left_deep_join (List.filter (function Unsupported _ -> false | _ -> true) nodes) in
      Some { nodes; ordered_where }
