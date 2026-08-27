(** Logical query plan IR and cost-based clause ordering (Phase 1–2 foundation).

    Unsupported shapes return [None] from [analyze]; callers fall back to the
    interpreter. [order_where_clauses] may still reorder supported pattern lists
    even when a full plan is unavailable. *)

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

(** Estimate how selective a single pattern is (lower is cheaper / narrower). *)
val estimate_pattern_cost : ?max_datom_e:int -> query_term -> query_term -> query_term -> int

(** Choose the preferred index for a ground/partial pattern. *)
val choose_index : query_term -> query_term -> query_term -> index_choice

(** Analyze a parsed query into a logical plan when all [:where] clauses are
    supported. Returns [None] when [:with] is present. *)
val analyze : ?max_datom_e:int -> query -> plan option

(** Cost-order pattern clauses while preserving relative order of non-patterns
    and of clauses that share the same estimated cost. *)
val order_where_clauses : ?max_datom_e:int -> query_clause list -> query_clause list
