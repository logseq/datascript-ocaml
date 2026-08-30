(** Datahike-aligned query planner: classify → logical IR → lower → physical ops.

    Unsupported / ineligible shapes return [None]; callers fall back to the
    relational interpreter (permanent fallback, matching Datahike). *)

open Datascript_types

type index_choice =
  | Prefer_eavt
  | Prefer_aevt
  | Prefer_avet

type l_scan =
  { entity : query_term
  ; attr : query_term
  ; value : query_term
  ; tx : query_term option
  ; source : string option
  ; clause : query_clause
  ; vars : string list
  }

type logical_node =
  | LScan of l_scan
  | LEntityJoin of
      { entity_var : string
      ; scans : l_scan list
      ; anti_scans : l_scan list
      ; filters : query_clause list
      ; source : string option
      }
  | LFilter of query_clause
  | LUnion of
      { join_vars : string list option
      ; branches : logical_plan list
      ; clause : query_clause
      }
  | LAntiJoin of
      { join_vars : string list option
      ; sub : logical_plan
      ; clause : query_clause
      }
  | LRuleExpand of
      { name : string
      ; terms : query_term list
      ; body : logical_plan
      }
  | LPassthrough of query_clause

and logical_plan =
  { nodes : logical_node list
  ; bound_vars : string list
  }

type entity_group =
  { entity_var : string
  ; scan : l_scan
  ; merges : l_scan list
  ; anti_scans : l_scan list
  ; filters : query_clause list
  ; clauses : query_clause list
  ; estimated_rows : int
  ; source : string option
  }

type physical_op =
  | OpEntityGroup of entity_group
  | OpScan of
      { clause : query_clause
      ; index : index_choice
      ; estimated_rows : int
      ; source : string option
      }
  | OpFilter of query_clause
  | OpUnion of
      { join_vars : string list option
      ; branches : physical_plan list
      }
  | OpAntiJoin of
      { join_vars : string list option
      ; excluded : physical_plan
      }
  | OpPassthrough of query_clause

and physical_plan =
  { ops : physical_op list
  }

let term_is_ground = function
  | QEntity _ | QIdent _ | QLookupRef _ | QAttr _ | QValue _ -> true
  | QVar _ | QSource _ | QWildcard -> false

let term_vars = function
  | QVar name -> [ name ]
  | QEntity _ | QIdent _ | QLookupRef _ | QAttr _ | QValue _ | QSource _ | QWildcard -> []

let terms_vars terms =
  terms |> List.concat_map term_vars |> List.sort_uniq compare

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

let make_scan ~source clause e a v tx =
  { entity = e
  ; attr = a
  ; value = v
  ; tx
  ; source
  ; clause
  ; vars = terms_vars (match tx with None -> [ e; a; v ] | Some tx -> [ e; a; v; tx ])
  }

let pattern_scan = function
  | Pattern (e, a, v) as clause -> Some (make_scan ~source:None clause e a v None)
  | PatternTx (e, a, v, tx) as clause -> Some (make_scan ~source:None clause e a v (Some tx))
  | SourcePattern (src, e, a, v) as clause -> Some (make_scan ~source:(Some src) clause e a v None)
  | SourcePatternTx (src, e, a, v, tx) as clause ->
    Some (make_scan ~source:(Some src) clause e a v (Some tx))
  | _ -> None

let filter_clause = function
  | ComparisonPredicate _ | EqualityPredicate _ | ComparisonPredicateN _ as clause -> Some clause
  | _ -> None

let entity_var_of_scan scan =
  match scan.entity with
  | QVar v -> Some v
  | _ -> None

(** Foldable NOT / NOT-JOIN: single pattern, same source, non-entity vars local to the negation. *)
let foldable_not_scan ~bound_vars ~var_owners clause_idx clause =
  let foldable_pattern = function
    | (Pattern (QVar e_var, QAttr _, value_term) as pattern) ->
      let local_vars =
        match value_term with
        | QVar v when v <> e_var -> [ v ]
        | _ -> []
      in
      let locals_ok =
        List.for_all
          (fun v ->
            (not (List.mem v bound_vars))
            &&
            match List.assoc_opt v var_owners with
            | None -> true
            | Some idxs -> List.for_all (( = ) clause_idx) idxs)
          local_vars
      in
      if locals_ok then pattern_scan pattern else None
    | _ -> None
  in
  match clause with
  | Not [ pattern ] -> foldable_pattern pattern
  | NotJoin ([ join_e ], [ pattern ]) -> (
    match foldable_pattern pattern with
    | Some anti_scan ->
      (match entity_var_of_scan anti_scan with
       | Some e_var when join_e = e_var -> Some anti_scan
       | _ -> None)
    | None -> None)
  | _ -> None

let var_owners_of_clauses clauses =
  let add owners idx var =
    match List.assoc_opt var owners with
    | Some idxs -> (var, idx :: idxs) :: List.remove_assoc var owners
    | None -> (var, [ idx ]) :: owners
  in
  clauses
  |> List.mapi (fun idx clause -> idx, clause)
  |> List.fold_left
       (fun owners (idx, clause) ->
         match pattern_scan clause with
         | Some scan -> List.fold_left (fun o v -> add o idx v) owners scan.vars
         | None ->
           (match filter_clause clause with
            | Some f ->
              (match f with
               | ComparisonPredicate (_, l, r) ->
                 List.fold_left (fun o v -> add o idx v) owners (terms_vars [ l; r ])
               | EqualityPredicate (_, terms) | ComparisonPredicateN (_, terms) ->
                 List.fold_left (fun o v -> add o idx v) owners (terms_vars terms)
               | _ -> owners)
            | None -> owners))
       []

let free_rule_body rules name arity =
  let matches =
    List.filter (fun rule -> rule.rule_name = name && List.length rule.rule_params = arity) rules
  in
  match matches with
  | [ rule ] ->
    (* Non-recursive: body must not call the same rule name. *)
    let rec body_calls_self = function
      | [] -> false
      | Rule (n, _) :: _ when n = name -> true
      | SourceRule (_, n, _) :: _ when n = name -> true
      | Not sub :: rest -> body_calls_self sub || body_calls_self rest
      | Or branches :: rest | OrJoin (_, branches) :: rest ->
        List.exists body_calls_self branches || body_calls_self rest
      | _ :: rest -> body_calls_self rest
    in
    if body_calls_self rule.rule_body then None else Some rule.rule_body
  | _ -> None

let rec build_logical_plan ?(max_datom_e = 1_000_000) ?(bound_vars = []) ?(rules = []) clauses =
  let _ = max_datom_e in
  let var_owners = var_owners_of_clauses clauses in
  let scans_and_rest =
    clauses
    |> List.mapi (fun idx clause -> idx, clause)
    |> List.fold_left
         (fun (scan_entries, other) (idx, clause) ->
           match pattern_scan clause with
           | Some scan -> ((idx, scan) :: scan_entries, other)
           | None -> (scan_entries, (idx, clause) :: other))
         ([], [])
  in
  let scan_entries, other_entries = scans_and_rest in
  let scan_entries = List.rev scan_entries in
  let other_entries = List.rev other_entries in
  (* Group scans by (entity_var, source). Ground-entity scans stay as LScan. *)
  let groups : ((string * string option) * l_scan list) list ref = ref [] in
  let ungrouped = ref [] in
  List.iter
    (fun (_idx, scan) ->
      match entity_var_of_scan scan with
      | None -> ungrouped := LScan scan :: !ungrouped
      | Some e_var ->
        let key = e_var, scan.source in
        (match List.assoc_opt key !groups with
         | Some existing -> groups := (key, scan :: existing) :: List.remove_assoc key !groups
         | None -> groups := (key, [ scan ]) :: !groups))
    scan_entries;
  let group_map =
    !groups
    |> List.map (fun ((e_var, source), scans) -> (e_var, source), List.rev scans)
  in
  (* Fold foldable NOTs into anti_scans only when a positive scan on the same
     entity already appears earlier in source order (DataScript outer-binding
     rules). Otherwise keep as LAntiJoin / passthrough so the interpreter can
     raise the same unbound-var errors. *)
  let remaining_other = ref [] in
  let anti_by_key : ((string * string option) * l_scan list) list ref = ref [] in
  let positive_entity_sources =
    scan_entries
    |> List.filter_map (fun (idx, scan) ->
      match entity_var_of_scan scan with
      | Some e_var -> Some (idx, (e_var, scan.source))
      | None -> None)
  in
  List.iter
    (fun (idx, clause) ->
      match foldable_not_scan ~bound_vars ~var_owners idx clause with
      | Some anti_scan ->
        (match entity_var_of_scan anti_scan with
         | Some e_var when List.mem_assoc (e_var, anti_scan.source) group_map ->
           let key = e_var, anti_scan.source in
           let has_earlier_positive =
             List.exists (fun (scan_idx, sk) -> sk = key && scan_idx < idx) positive_entity_sources
           in
           if has_earlier_positive then
             match List.assoc_opt key !anti_by_key with
             | Some existing ->
               anti_by_key := (key, anti_scan :: existing) :: List.remove_assoc key !anti_by_key
             | None -> anti_by_key := (key, [ anti_scan ]) :: !anti_by_key
           else
             remaining_other := (idx, clause) :: !remaining_other
         | _ -> remaining_other := (idx, clause) :: !remaining_other)
      | None -> remaining_other := (idx, clause) :: !remaining_other)
    other_entries;
  let remaining_other = List.rev !remaining_other in
  (* Attach comparison filters whose vars ⊆ one entity group's vars *)
  let filters_by_key : ((string * string option) * query_clause list) list ref = ref [] in
  let leftover = ref [] in
  List.iter
    (fun (_idx, clause) ->
      match filter_clause clause with
      | Some filter ->
        let fvars =
          match filter with
          | ComparisonPredicate (_, l, r) -> terms_vars [ l; r ]
          | EqualityPredicate (_, terms) | ComparisonPredicateN (_, terms) -> terms_vars terms
          | _ -> []
        in
        let owner =
          group_map
          |> List.find_map (fun (((e_var, _source) as key), scans) ->
            let gvars =
              e_var
              :: (scans |> List.concat_map (fun s -> s.vars))
              |> List.sort_uniq compare
            in
            if fvars <> [] && List.for_all (fun v -> List.mem v gvars) fvars then
              Some key
            else
              None)
        in
        (match owner with
         | Some key ->
           (match List.assoc_opt key !filters_by_key with
            | Some existing ->
              filters_by_key := (key, filter :: existing) :: List.remove_assoc key !filters_by_key
            | None -> filters_by_key := (key, [ filter ]) :: !filters_by_key)
         | None -> leftover := clause :: !leftover)
      | None -> leftover := clause :: !leftover)
    remaining_other;
  let leftover = List.rev !leftover in
  let entity_nodes =
    group_map
    |> List.map (fun (((e_var, source) as key), scans) ->
      let anti = Option.value (List.assoc_opt key !anti_by_key) ~default:[] |> List.rev in
      let filters = Option.value (List.assoc_opt key !filters_by_key) ~default:[] |> List.rev in
      match scans, anti, filters with
      | [ single ], [], [] -> LScan single
      | _ ->
        LEntityJoin
          { entity_var = e_var; scans; anti_scans = anti; filters; source })
  in
  let other_nodes_opt =
    leftover
    |> List.fold_left
         (fun acc clause ->
           match acc with
           | None -> None
           | Some nodes ->
             (match clause with
              | Or branches as c ->
                let branch_plans =
                  List.map
                    (fun branch -> build_logical_plan ~max_datom_e ~bound_vars ~rules branch)
                    branches
                in
                if List.for_all Option.is_some branch_plans then
                  Some
                    (LUnion
                       { join_vars = None
                       ; branches = List.filter_map Fun.id branch_plans
                       ; clause = c
                       }
                     :: nodes)
                else
                  Some (LPassthrough c :: nodes)
              | OrJoin (vars, branches) as c ->
                let branch_plans =
                  List.map
                    (fun branch -> build_logical_plan ~max_datom_e ~bound_vars ~rules branch)
                    branches
                in
                if List.for_all Option.is_some branch_plans then
                  Some
                    (LUnion
                       { join_vars = Some vars
                       ; branches = List.filter_map Fun.id branch_plans
                       ; clause = c
                       }
                     :: nodes)
                else
                  Some (LPassthrough c :: nodes)
              | Not sub as c ->
                (match build_logical_plan ~max_datom_e ~bound_vars ~rules sub with
                 | Some sub_plan ->
                   Some (LAntiJoin { join_vars = None; sub = sub_plan; clause = c } :: nodes)
                 | None -> Some (LPassthrough c :: nodes))
              | NotJoin (vars, sub) as c ->
                (match build_logical_plan ~max_datom_e ~bound_vars ~rules sub with
                 | Some sub_plan ->
                   Some (LAntiJoin { join_vars = Some vars; sub = sub_plan; clause = c } :: nodes)
                 | None -> Some (LPassthrough c :: nodes))
              | Rule (name, terms) as c ->
                (match free_rule_body rules name (List.length terms) with
                 | Some body ->
                   (match build_logical_plan ~max_datom_e ~bound_vars ~rules body with
                    | Some body_plan ->
                      Some (LRuleExpand { name; terms; body = body_plan } :: nodes)
                    | None -> Some (LPassthrough c :: nodes))
                 | None -> Some (LPassthrough c :: nodes))
              | ComparisonPredicate _ | EqualityPredicate _ | ComparisonPredicateN _ as c ->
                Some (LFilter c :: nodes)
              | c -> Some (LPassthrough c :: nodes)))
         (Some [])
  in
  match other_nodes_opt with
  | None -> None
  | Some other_nodes ->
    Some
      { nodes = List.rev_append entity_nodes (List.rev_append !ungrouped (List.rev other_nodes))
      ; bound_vars
      }

let scan_estimated_rows ~max_datom_e scan =
  estimate_pattern_cost ~max_datom_e scan.entity scan.attr scan.value

let entity_group_cost ~max_datom_e scans =
  match scans with
  | [] -> max_datom_e
  | _ ->
    scans
    |> List.map (scan_estimated_rows ~max_datom_e)
    |> List.fold_left min max_datom_e

let op_cost = function
  | OpEntityGroup { estimated_rows; _ } | OpScan { estimated_rows; _ } -> estimated_rows
  | OpFilter _ -> 50
  | OpUnion _ -> 900_000
  | OpAntiJoin _ -> 1_000_000
  | OpPassthrough _ -> 2_000_000

let op_produced_vars = function
  | OpEntityGroup { clauses; _ } ->
    clauses
    |> List.concat_map (fun clause ->
      match pattern_scan clause with
      | Some s -> s.vars
      | None -> [])
    |> List.sort_uniq compare
  | OpScan { clause; _ } ->
    (match pattern_scan clause with Some s -> s.vars | None -> [])
  | OpFilter _ | OpUnion _ | OpAntiJoin _ | OpPassthrough _ -> []

let filter_required_vars = function
  | ComparisonPredicate (_, l, r) -> terms_vars [ l; r ]
  | EqualityPredicate (_, terms) | ComparisonPredicateN (_, terms) -> terms_vars terms
  | _ -> []

let rec lower_node ~max_datom_e = function
  | LScan scan ->
    OpScan
      { clause = scan.clause
      ; index = choose_index scan.entity scan.attr scan.value
      ; estimated_rows = scan_estimated_rows ~max_datom_e scan
      ; source = scan.source
      }
  | LEntityJoin { entity_var; scans; anti_scans; filters; source } ->
    let ordered_scans =
      scans
      |> List.mapi (fun i s -> scan_estimated_rows ~max_datom_e s, i, s)
      |> List.sort (fun (c1, i1, _) (c2, i2, _) ->
        let cmp = compare c1 c2 in
        if cmp <> 0 then cmp else compare i1 i2)
      |> List.map (fun (_, _, s) -> s)
    in
    let scan, merges =
      match ordered_scans with
      | [] -> invalid_arg "entity join requires at least one scan"
      | driving :: rest -> driving, rest
    in
    let pattern_clauses = List.map (fun s -> s.clause) ordered_scans in
    let anti_clauses = List.map (fun s -> Not [ s.clause ]) anti_scans in
    OpEntityGroup
      { entity_var
      ; scan
      ; merges
      ; anti_scans
      ; filters
      ; clauses = pattern_clauses @ anti_clauses @ filters
      ; estimated_rows = entity_group_cost ~max_datom_e ordered_scans
      ; source
      }
  | LFilter clause -> OpFilter clause
  | LUnion { join_vars; branches; _ } ->
    let branch_plans = List.filter_map (lower ~max_datom_e) branches in
    if List.length branch_plans <> List.length branches then
      OpPassthrough (Or [])
    else
      OpUnion { join_vars; branches = branch_plans }
  | LAntiJoin { join_vars; sub; clause } ->
    (match lower ~max_datom_e sub with
     | Some excluded -> OpAntiJoin { join_vars; excluded }
     | None -> OpPassthrough clause)
  | LRuleExpand { body; _ } ->
    (* Prefer single-op bodies; multi-op rule bodies fall through to interpreter. *)
    (match lower ~max_datom_e body with
     | Some { ops = [ op ] } -> op
     | _ -> OpPassthrough (Rule ("", [])))
  | LPassthrough clause -> OpPassthrough clause

and lower ?(max_datom_e = 1_000_000) logical =
  let raw_ops = List.map (lower_node ~max_datom_e) logical.nodes in
  (* Expand single-op rule inlines already done; multi-op rule markers stay passthrough. *)
  let ops = raw_ops in
  let rec schedule ready_vars remaining scheduled =
    match remaining with
    | [] -> List.rev scheduled
    | _ ->
      let indexed = List.mapi (fun i op -> i, op) remaining in
      let ready_idxs =
        indexed
        |> List.filter_map (fun (i, op) ->
          match op with
          | OpFilter clause ->
            let req = filter_required_vars clause in
            if List.for_all (fun v -> List.mem v ready_vars || List.mem v logical.bound_vars) req
            then Some i
            else None
          | OpAntiJoin _ ->
            if scheduled <> [] || ready_vars <> [] || logical.bound_vars <> [] then Some i else None
          | OpPassthrough _ -> None
          | _ -> Some i)
      in
      let pick_idx =
        match ready_idxs with
        | [] ->
          (match
             indexed
             |> List.sort (fun (_, a) (_, b) -> compare (op_cost a) (op_cost b))
           with
           | (i, _) :: _ -> Some i
           | [] -> None)
        | idxs ->
          idxs
          |> List.map (fun i -> op_cost (List.nth remaining i), i)
          |> List.sort (fun (c1, i1) (c2, i2) ->
            let cmp = compare c1 c2 in
            if cmp <> 0 then cmp else compare i1 i2)
          |> fun sorted -> Some (snd (List.hd sorted))
      in
      (match pick_idx with
       | None -> List.rev_append scheduled remaining
       | Some idx ->
         let first = List.nth remaining idx in
         let rest = List.filteri (fun i _ -> i <> idx) remaining in
         let ready_vars = List.sort_uniq compare (ready_vars @ op_produced_vars first) in
         schedule ready_vars rest (first :: scheduled))
  in
  let scans, others =
    List.partition
      (function OpEntityGroup _ | OpScan _ | OpUnion _ -> true | _ -> false)
      ops
  in
  let scans =
    scans
    |> List.mapi (fun i op -> op_cost op, i, op)
    |> List.sort (fun (c1, i1, _) (c2, i2, _) ->
      let cmp = compare c1 c2 in
      if cmp <> 0 then cmp else compare i1 i2)
    |> List.map (fun (_, _, op) -> op)
  in
  let initial = schedule [] (scans @ others) [] in
  let ops =
    match initial with
    | (OpAntiJoin _ as anti) :: rest ->
      (match
         List.find_index
           (function OpEntityGroup _ | OpScan _ | OpUnion _ -> true | _ -> false)
           rest
       with
       | None -> initial
       | Some idx ->
         let before = List.filteri (fun i _ -> i < idx) rest in
         (match List.filteri (fun i _ -> i >= idx) rest with
          | [] -> initial
          | producer :: after -> before @ (producer :: anti :: after)))
    | _ -> initial
  in
  Some { ops }

(** Substitute rule params with call terms inside a body clause list. *)
let rec substitute_rule_terms param_map = function
  | [] -> []
  | clause :: rest ->
    let subst_term = function
      | QVar name -> (match List.assoc_opt name param_map with Some t -> t | None -> QVar name)
      | other -> other
    in
    let subst_clause = function
      | Pattern (e, a, v) -> Pattern (subst_term e, subst_term a, subst_term v)
      | PatternTx (e, a, v, tx) -> PatternTx (subst_term e, subst_term a, subst_term v, subst_term tx)
      | ComparisonPredicate (p, l, r) -> ComparisonPredicate (p, subst_term l, subst_term r)
      | EqualityPredicate (p, terms) -> EqualityPredicate (p, List.map subst_term terms)
      | Not sub -> Not (substitute_rule_terms param_map sub)
      | NotJoin (vars, sub) -> NotJoin (vars, substitute_rule_terms param_map sub)
      | Or branches -> Or (List.map (substitute_rule_terms param_map) branches)
      | OrJoin (vars, branches) -> OrJoin (vars, List.map (substitute_rule_terms param_map) branches)
      | Rule (name, terms) -> Rule (name, List.map subst_term terms)
      | other -> other
    in
    subst_clause clause :: substitute_rule_terms param_map rest

let expand_nonrecursive_rules rules clauses =
  let rec expand depth clauses =
    if depth > 8 then None
    else
      let rec loop acc = function
        | [] -> Some (List.rev acc)
        | Rule (name, terms) :: rest ->
          (match free_rule_body rules name (List.length terms) with
           | None -> None
           | Some body ->
             let params =
               match
                 List.find_opt
                   (fun r -> r.rule_name = name && List.length r.rule_params = List.length terms)
                   rules
               with
               | Some r -> r.rule_params
               | None -> []
             in
             let param_map = List.combine params terms in
             let body = substitute_rule_terms param_map body in
             (match expand (depth + 1) body with
              | None -> None
              | Some expanded -> loop (List.rev_append expanded acc) rest))
        | clause :: rest -> loop (clause :: acc) rest
      in
      loop [] clauses
  in
  expand 0 clauses

let compile ?(max_datom_e = 1_000_000) ?(bound_vars = []) ?(rules = []) clauses =
  match expand_nonrecursive_rules rules clauses with
  | None ->
    (* Keep rule clauses; logical plan may mark them passthrough. *)
    (match build_logical_plan ~max_datom_e ~bound_vars ~rules clauses with
     | None -> None
     | Some logical -> lower ~max_datom_e logical)
  | Some expanded ->
    (match build_logical_plan ~max_datom_e ~bound_vars ~rules expanded with
     | None -> None
     | Some logical -> lower ~max_datom_e logical)

let analyze ?(max_datom_e = 1_000_000) ?(bound_vars = []) ?(rules = []) query =
  if query.with_vars <> [] then None
  else compile ~max_datom_e ~bound_vars ~rules:(rules @ query.rules) query.where

let plan_is_executable plan =
  not (List.exists (function OpPassthrough _ -> true | _ -> false) plan.ops)

(* Single ground scan or entity-group: Query_exec emits directly.
   Multi entity-group joins (e.g. cross-entity value join) stay on the
   relational path, which has a selective AVET/AEVT specialized join. *)
let plan_is_fused_execute plan =
  match plan.ops with
  | [ OpEntityGroup _ ] | [ OpScan _ ] -> true
  | _ -> false

let rec clauses_of_plan plan =
  plan.ops
  |> List.concat_map (function
    | OpEntityGroup { clauses; _ } -> clauses
    | OpScan { clause; _ } -> [ clause ]
    | OpFilter clause -> [ clause ]
    | OpUnion { join_vars = None; branches } ->
      [ Or (List.map clauses_of_plan branches) ]
    | OpUnion { join_vars = Some vars; branches } ->
      [ OrJoin (vars, List.map clauses_of_plan branches) ]
    | OpAntiJoin { join_vars = None; excluded } -> [ Not (clauses_of_plan excluded) ]
    | OpAntiJoin { join_vars = Some vars; excluded } ->
      [ NotJoin (vars, clauses_of_plan excluded) ]
    | OpPassthrough clause -> [ clause ])
