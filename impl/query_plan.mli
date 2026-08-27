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

type physical_op =
  | OpEntityGroup of
      { entity_var : string
      ; clauses : query_clause list
      ; estimated_rows : int
      ; source : string option
      }
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

(** Ground-component index preference (Datahike plan-pattern-op). *)
val choose_index : query_term -> query_term -> query_term -> index_choice

(** Cardinality estimate for a pattern. Prefer tighter constants over open scans. *)
val estimate_pattern_cost : ?max_datom_e:int -> query_term -> query_term -> query_term -> int

(** Build unordered logical plan; [None] when [:with] or unsupported top-level shape. *)
val build_logical_plan :
  ?max_datom_e:int -> ?bound_vars:string list -> ?rules:query_rule list -> query_clause list -> logical_plan option

(** Lower logical plan to ordered physical ops with readiness-aware cost order. *)
val lower : ?max_datom_e:int -> logical_plan -> physical_plan option

(** Compile where-clauses: logical → lower. *)
val compile :
  ?max_datom_e:int -> ?bound_vars:string list -> ?rules:query_rule list -> query_clause list -> physical_plan option

(** Analyze a full query into a physical plan when eligible. *)
val analyze : ?max_datom_e:int -> ?bound_vars:string list -> ?rules:query_rule list -> query -> physical_plan option

(** True when every op is planner-executable (no [OpPassthrough]). *)
val plan_is_executable : physical_plan -> bool

(** Flatten a physical plan back to where-clauses in execution order (tests / explain). *)
val clauses_of_plan : physical_plan -> query_clause list
