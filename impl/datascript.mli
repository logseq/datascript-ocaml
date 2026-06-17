type entity_id = Datascript_types.entity_id
type attr = Datascript_types.attr
type tx = Datascript_types.tx

type entity_ref = Datascript_types.entity_ref =
  | Entity_id of entity_id
  | Temp_id of string
  | CurrentTx
  | Ident of string
  | Lookup_ref of attr * value

and value = Datascript_types.value =
  | Nil
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Bool of bool
  | Keyword of string
  | Uuid of string
  | Instant of int
  | Regex of string
  | Ref of entity_id
  | List of value list
  | Map of (value * value) list
  | Set of value list
  | Tuple of value option list
  | TxRef
  | Ref_to of entity_ref

type cardinality = Datascript_types.cardinality =
  | One
  | Many

type unique = Datascript_types.unique =
  | Value
  | Identity

type value_type = Datascript_types.value_type =
  | RefType
  | TupleType
  | StringType
  | KeywordType
  | NumberType
  | UuidType
  | InstantType

type schema_attr = Datascript_types.schema_attr =
  { cardinality : cardinality
  ; unique : unique option
  ; indexed : bool
  ; is_component : bool
  ; no_history : bool
  ; doc : string option
  ; value_type : value_type option
  ; tuple_attrs : attr list option
  ; tuple_types : value_type list option
  }

type schema = Datascript_types.schema

type datom = Datascript_types.datom =
  { e : entity_id
  ; a : attr
  ; v : value
  ; tx : tx
  ; added : bool
  }

type tx_value = Datascript_types.tx_value =
  | One_value of value
  | Many_values of value list
  | One_entity of tx_entity
  | Many_entities of tx_entity list

and tx_entity = Datascript_types.tx_entity =
  { db_id : entity_ref option
  ; attrs : (attr * tx_value) list
  }

type db = Datascript_types.db

type tx_op = Datascript_types.tx_op =
  | Add of entity_ref * attr * value
  | Retract of entity_ref * attr * value option
  | RetractEntity of entity_ref
  | RetractAttr of entity_ref * attr
  | CompareAndSet of entity_ref * attr * value option * value
  | Entity of tx_entity
  | Raw_datom of datom
  | InstallTxFn of entity_ref * (db -> value list -> tx_op list)
  | CallIdent of entity_ref * value list
  | Call of (db -> tx_op list)

type entity = Datascript_types.entity =
  { id : entity_id
  ; db : db
  ; attrs : (attr * tx_value) list
  }

type pulled_entity = Datascript_types.pulled_entity =
  { pulled_id : entity_id
  ; pulled_attrs : (pull_key * pulled_value) list
  }

and pull_key = Datascript_types.pull_key

and pulled_value = Datascript_types.pulled_value =
  | Pulled_scalar of value
  | Pulled_many of pulled_value list
  | Pulled_entity of pulled_entity

type pull_visit = Datascript_types.pull_visit =
  | PullVisitAttr of entity_id * attr
  | PullVisitWildcard of entity_id
  | PullVisitReverse of attr * entity_id

type pull_selector = Datascript_types.pull_selector =
  | Pull_id
  | Pull_wildcard
  | Pull_attr of attr
  | Pull_attr_default of attr * value
  | Pull_attr_limit of attr * int
  | Pull_attr_unlimited of attr
  | Pull_attr_xform of attr * (pulled_value -> pulled_value)
  | Pull_attr_default_xform of attr * value * (pulled_value -> pulled_value)
  | Pull_ref of attr * pull_selector list
  | Pull_ref_default of attr * pull_selector list * value
  | Pull_ref_limit of attr * pull_selector list * int
  | Pull_ref_unlimited of attr * pull_selector list
  | Pull_ref_xform of attr * pull_selector list * (pulled_value -> pulled_value)
  | Pull_recursive_ref of attr * pull_selector list * int option
  | Pull_reverse_ref of attr * pull_selector list
  | Pull_reverse_ref_default of attr * pull_selector list * value
  | Pull_reverse_ref_limit of attr * pull_selector list * int
  | Pull_reverse_ref_unlimited of attr * pull_selector list
  | Pull_reverse_ref_xform of attr * pull_selector list * (pulled_value -> pulled_value)
  | Pull_as of pull_selector * pull_key

type query_term = Datascript_types.query_term =
  | QVar of string
  | QEntity of entity_id
  | QIdent of string
  | QLookupRef of attr * value
  | QAttr of attr
  | QValue of value
  | QSource of string
  | QWildcard

type query_result = Datascript_types.query_result =
  | Result_entity of entity_id
  | Result_attr of attr
  | Result_value of value
  | Result_db of db
  | Result_pull of pulled_entity

type query_source = Datascript_types.query_source =
  | Db_source of db
  | Relation_source of query_result list list

type value_predicate = Datascript_types.value_predicate =
  | NumberValue
  | IntegerValue
  | StringValue
  | BooleanValue
  | KeywordValue

type numeric_predicate = Datascript_types.numeric_predicate =
  | ZeroNumber
  | PositiveNumber
  | NegativeNumber
  | EvenInteger
  | OddInteger

type comparison_predicate = Datascript_types.comparison_predicate =
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual

type equality_predicate = Datascript_types.equality_predicate =
  | EqualValues
  | NotEqualValues

type arithmetic_op = Datascript_types.arithmetic_op =
  | AddNumbers
  | SubtractNumbers
  | MultiplyNumbers
  | DivideNumbers
  | IncrementNumber
  | DecrementNumber
  | QuotientNumbers
  | RemainderNumbers
  | ModuloNumbers

type extremum_op = Datascript_types.extremum_op =
  | MinimumValue
  | MaximumValue

type boolean_predicate = Datascript_types.boolean_predicate =
  | TrueValue
  | FalseValue
  | NilValue
  | SomeValue

type query_clause = Datascript_types.query_clause =
  | Pattern of query_term * query_term * query_term
  | PatternTx of query_term * query_term * query_term * query_term
  | PatternTxOp of query_term * query_term * query_term * query_term * query_term
  | SourcePattern of string * query_term * query_term * query_term
  | SourcePatternTx of string * query_term * query_term * query_term * query_term
  | SourcePatternTxOp of string * query_term * query_term * query_term * query_term * query_term
  | SourceRelationPattern of string * query_term list
  | Missing of query_term * attr
  | SourceMissing of string * query_term * attr
  | GetElse of query_term * attr * value * string
  | SourceGetElse of string * query_term * attr * value * string
  | GetSome of query_term * attr list * string * string
  | SourceGetSome of string * query_term * attr list * string * string
  | GetValue of query_term * query_term * string
  | GetDefaultValue of query_term * query_term * query_term * string
  | CountValue of query_term * string
  | EmptyValue of query_term
  | NotEmptyValue of query_term
  | ContainsValue of query_term * query_term
  | ValuePredicate of value_predicate * query_term
  | NumericPredicate of numeric_predicate * query_term
  | ComparisonPredicate of comparison_predicate * query_term * query_term
  | ComparisonPredicateN of comparison_predicate * query_term list
  | EqualityPredicate of equality_predicate * query_term list
  | ArithmeticValue of arithmetic_op * query_term list * string
  | CompareValue of query_term * query_term * string
  | ExtremumValue of extremum_op * query_term list * string
  | BooleanPredicate of boolean_predicate * query_term
  | BooleanNotPredicate of query_term
  | BooleanNotValue of query_term * string
  | IdentityValue of query_term * string
  | BooleanAndPredicate of query_term list
  | BooleanAndValue of query_term list * string
  | BooleanOrPredicate of query_term list
  | BooleanOrValue of query_term list * string
  | RandomValue of string
  | RandomIntValue of query_term * string
  | DifferPredicate of query_term list
  | IdenticalPredicate of query_term * query_term
  | TypeValue of query_term * string
  | MetaValue of query_term * string
  | NameValue of query_term * string
  | NamespaceValue of query_term * string
  | KeywordFromName of query_term * string
  | KeywordFromNamespaceName of query_term * query_term * string
  | StringIncludesValue of query_term * query_term
  | StringStartsWithValue of query_term * query_term
  | StringEndsWithValue of query_term * query_term
  | StringLowerCaseValue of query_term * string
  | StringUpperCaseValue of query_term * string
  | StringCapitalizeValue of query_term * string
  | StringReverseValue of query_term * string
  | StringTrimValue of query_term * string
  | StringTrimLeftValue of query_term * string
  | StringTrimRightValue of query_term * string
  | StringTrimNewlineValue of query_term * string
  | StringIndexOfValue of query_term * query_term * string
  | StringLastIndexOfValue of query_term * query_term * string
  | StringSubstringValue of query_term * query_term * query_term option * string
  | StringBuildValue of query_term list * string
  | PrintStringValue of query_term list * string
  | PrintLineStringValue of query_term list * string
  | PrStringValue of query_term list * string
  | PrnStringValue of query_term list * string
  | StringJoinPlainValue of query_term * string
  | StringJoinValue of query_term * query_term * string
  | StringReplaceValue of query_term * query_term * query_term * string
  | StringReplaceFirstValue of query_term * query_term * query_term * string
  | StringEscapeValue of query_term * query_term * string
  | RePatternValue of query_term * string
  | ReFindValue of query_term * query_term * string
  | ReMatchesValue of query_term * query_term * string
  | ReSeqValue of query_term * query_term * string
  | StringBlankValue of query_term
  | StringSplitValue of query_term * query_term * string
  | StringSplitLimitValue of query_term * query_term * query_term * string
  | StringSplitLinesValue of query_term * string
  | Ground of value * string
  | GroundCollection of value list * string
  | GroundTuple of value list * string list
  | GroundRelation of value list list * string list
  | GroundTerm of query_term * string
  | GroundTermCollection of query_term * string
  | GroundTermTuple of query_term * string list
  | GroundTermRelation of query_term * string list
  | VectorValue of query_term list * string
  | ListValue of query_term list * string
  | SetValue of query_term list * string
  | HashMapValue of query_term list * string
  | ArrayMapValue of query_term list * string
  | RangeEndValue of query_term * string
  | RangeValue of query_term * query_term * string
  | RangeStepValue of query_term * query_term * query_term * string
  | TupleFunction of query_term list * string
  | UntupleFunction of query_term * string list
  | Predicate of string * query_term list * (query_result list -> bool)
  | Function of string * query_term list * string list * (query_result list -> query_result list option)
  | DynamicPredicate of string * query_term list
  | DynamicFunction of string * query_term list * string list
  | DynamicFunctionCollection of string * query_term list * string
  | DynamicFunctionRelation of string * query_term list * string list
  | SourceClause of string * query_clause
  | Not of query_clause list
  | SourceNot of string * query_clause list
  | NotJoin of string list * query_clause list
  | SourceNotJoin of string * string list * query_clause list
  | Or of query_clause list list
  | SourceOr of string * query_clause list list
  | OrJoin of string list * query_clause list list
  | SourceOrJoin of string * string list * query_clause list list
  | OrJoinRequired of string list * string list * query_clause list list
  | SourceOrJoinRequired of string * string list * string list * query_clause list list
  | Rule of string * query_term list
  | SourceRule of string * string * query_term list

type query_rule = Datascript_types.query_rule =
  { rule_name : string
  ; rule_params : string list
  ; rule_body : query_clause list
  }

type input_binding = Datascript_types.input_binding =
  | Bind_scalar of string
  | Bind_ignore
  | Bind_collection of input_binding
  | Bind_tuple of input_binding list

type query_input = Datascript_types.query_input =
  | Input_scalar of string * query_result
  | Input_entity_ref of string * entity_ref
  | Input_collection of string * query_result list
  | Input_collection_ignore of query_result list
  | Input_nested_collection of input_binding * query_result list
  | Input_tuple of string list * query_result list
  | Input_relation of string list * query_result list list
  | Input_nested_tuple of input_binding list * query_result list
  | Input_nested_relation of input_binding list * query_result list list
  | Input_predicate of string * (query_result list -> bool)
  | Input_function of string * (query_result list -> query_result list option)
  | Input_aggregate of string * (query_result list -> query_result)
  | Input_rules of query_rule list
  | Input_ignore
  | Input_scalar_decl of string
  | Input_collection_decl of string
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_rules_decl
  | Input_nested_collection_decl of input_binding
  | Input_tuple_decl of string list
  | Input_relation_decl of string list
  | Input_nested_tuple_decl of input_binding list
  | Input_nested_relation_decl of input_binding list
  | Input_source_decl of string

type query_arg = Datascript_types.query_arg =
  | Arg_scalar of query_result
  | Arg_entity_ref of entity_ref
  | Arg_collection of query_result list
  | Arg_tuple of query_result list
  | Arg_relation of query_result list list
  | Arg_predicate of (query_result list -> bool)
  | Arg_function of (query_result list -> query_result list option)
  | Arg_aggregate of (query_result list -> query_result)
  | Arg_rules of query_rule list

type aggregate = Datascript_types.aggregate =
  | Count
  | CountDistinct
  | Distinct
  | Sum
  | Avg
  | Median
  | Variance
  | Stddev
  | Min
  | Max
  | MinN of int
  | MaxN of int
  | Rand
  | RandN of int
  | Sample of int
  | MinNVar of string
  | MaxNVar of string
  | RandNVar of string
  | SampleVar of string
  | CustomVar of string
  | Custom of (query_result list -> query_result)

type find_spec = Datascript_types.find_spec =
  | Find_var of string
  | Find_pull of string * pull_selector list
  | Find_pull_var of string * string
  | Find_pull_source of string * string * pull_selector list
  | Find_pull_source_var of string * string * string
  | Find_aggregate of aggregate * query_term list

type query = Datascript_types.query =
  { find : find_spec list
  ; inputs : query_input list
  ; with_vars : string list
  ; rules : query_rule list
  ; where : query_clause list
  }

type query_form = Datascript_types.query_form =
  | QueryFormNil
  | QueryFormBool of bool
  | QueryFormInt of int
  | QueryFormFloat of float
  | QueryFormString of string
  | QueryFormKeyword of string
  | QueryFormSymbol of string
  | QueryFormVector of query_form list
  | QueryFormList of query_form list
  | QueryFormSet of query_form list
  | QueryFormTagged of string * query_form
  | QueryFormMap of (query_form * query_form) list

type query_return = Datascript_types.query_return =
  | Return_relation
  | Return_collection
  | Return_tuple
  | Return_scalar

type query_return_map = Datascript_types.query_return_map =
  | Return_keys of string list
  | Return_syms of string list
  | Return_strs of string list

type query_output = Datascript_types.query_output =
  | Query_relation of query_result list list
  | Query_collection of query_result list
  | Query_tuple of query_result list option
  | Query_scalar of query_result option
  | Query_relation_maps of (value * query_result) list list
  | Query_tuple_map of (value * query_result) list option

type index = Datascript_types.index =
  | Eavt
  | Aevt
  | Avet
  | Vaet

type serializable_db = Datascript_types.serializable_db =
  { serializable_schema : schema
  ; serializable_datoms : datom list
  ; serializable_history_datoms : datom list
  ; serializable_historical : bool
  ; serializable_max_eid : entity_id
  ; serializable_max_tx : tx
  }

type storage_address = Datascript_types.storage_address

type storage_payload = Datascript_types.storage_payload =
  | Storage_db of serializable_db
  | Storage_tail of datom list list

type storage = Datascript_types.storage =
  { storage_store : (storage_address * storage_payload) list -> storage_address list -> unit
  ; storage_restore : storage_address -> storage_payload option
  ; storage_list_addresses : unit -> storage_address list
  ; storage_delete : storage_address list -> unit
  }

type tx_meta = Datascript_types.tx_meta

type tx_report = Datascript_types.tx_report =
  { db_before : db
  ; db_after : db
  ; tx_data : datom list
  ; tempids : (string * entity_id) list
  ; tx_meta : tx_meta
  }

type conn

module Lru : sig
  type ('key, 'value) t
  type ('key, 'value) cache

  val create : int -> ('key, 'value) t
  val assoc : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val find : 'key -> ('key, 'value) t -> 'value option
  val cache : int -> ('key, 'value) cache
  val cache_get : ('key, 'value) cache -> 'key -> (unit -> 'value) -> 'value
end

val tx0 : tx
val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
val is_datom : datom -> bool
val empty_db : ?schema:schema -> ?storage:storage -> unit -> db
val is_db : db -> bool
val init_db : ?schema:schema -> ?storage:storage -> datom list -> db
val history : db -> db
val is_history : db -> bool
val filter : db -> (db -> datom -> bool) -> db
val is_filtered : db -> bool
val unfiltered_db : db -> db
val serializable : db -> serializable_db
val from_serializable : serializable_db -> db
val db_from_reader_string : string -> db
val memory_storage : unit -> storage
val file_storage : string -> storage
val store : ?storage:storage -> db -> unit
val store_tail : storage -> datom list list -> unit
val restore : storage -> db option
val db_with_tail : db -> datom list list -> db
val storage : db -> storage option
val addresses : db list -> storage_address list
val settings : db -> (attr * value) list
val storage_addresses : storage -> storage_address list
val collect_garbage : storage -> unit
val diff : db -> db -> datom list * datom list * datom list
val squuid : ?msec:int -> unit -> value
val squuid_time_millis : value -> int
val create_conn : ?schema:schema -> ?storage:storage -> unit -> conn
val conn_from_db : db -> conn
val conn_from_datoms : ?schema:schema -> ?storage:storage -> datom list -> conn
val restore_conn : storage -> conn option
val conn_db : conn -> db
val db : conn -> db
val is_conn : conn -> bool
val listen : conn -> string -> (tx_report -> unit) -> string
val listen_bang : conn -> string -> (tx_report -> unit) -> string
val listen_auto : conn -> (tx_report -> unit) -> string
val listen_bang_auto : conn -> (tx_report -> unit) -> string
val unlisten : conn -> string -> unit
val unlisten_bang : conn -> string -> unit
val reset_conn : ?tx_meta:tx_meta -> conn -> db -> db
val reset_conn_bang : ?tx_meta:tx_meta -> conn -> db -> db
val reset_schema : conn -> schema -> db
val reset_schema_bang : conn -> schema -> db
val schema : db -> schema
val with_schema : db -> schema -> db
val schema_of_edn_string : string -> schema
val is_reverse_ref : attr -> bool
val reverse_ref : attr -> attr
val parse_tx_data_string : string -> tx_op list
val db_with : tx_op list -> db -> db
val db_with_string : string -> db -> db
val transact : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
val transact_string : ?tx_meta:tx_meta -> db -> string -> tx_report
val with_tx : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
val with_tx_string : ?tx_meta:tx_meta -> db -> string -> tx_report
val transact_conn : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_conn_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val transact_bang : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_bang_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val transact_async : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_async_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val tempid : ?part:string -> ?value:int -> unit -> entity_ref
val resolve_tempid : ?db:db -> (string * entity_id) list -> string -> entity_id option
val entity : db -> entity_ref -> entity option
val entity_attr : entity -> attr -> tx_value option
val entity_db : entity -> db
val is_entity : entity -> bool
val touch : entity -> entity
val entid : db -> attr -> value -> entity_id option
val entid_ref : db -> entity_ref -> entity_id option
val read_edn : string -> query_form
val parse_binding : query_form -> input_binding
val parse_in : query_form -> query_input list
val parse_with : query_form -> string list
val parse_find : query_form -> query_return * find_spec list
val parse_pull_pattern : db -> query_form -> pull_selector list
val parse_pull_pattern_string : db -> string -> pull_selector list
val pull : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref -> pulled_entity option
val pull_string : ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref -> pulled_entity option
val pull_many : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref list -> pulled_entity option list
val pull_many_string : ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref list -> pulled_entity option list
val parse_query : query_form -> query
val parse_query_string : string -> query
val parse_query_return : query_form -> query_return * query
val parse_query_return_string : string -> query_return * query
val parse_query_return_map : query_form -> query_return * query_return_map option * query
val parse_query_return_map_string : string -> query_return * query_return_map option * query
val q : ?inputs:query_arg list -> db -> query -> query_result list list
val q_string : ?inputs:query_arg list -> db -> string -> query_result list list
val q_with : ?inputs:query_arg list -> db -> string list -> query -> query_result list list
val q_with_string :
  ?inputs:query_arg list -> db -> string list -> string -> query_result list list
val q_sources :
  ?inputs:query_arg list -> db -> (string * query_source) list -> query -> query_result list list
val q_sources_string :
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  string ->
  query_result list list
val q_return : ?inputs:query_arg list -> db -> query_return -> query -> query_output
val q_return_string : ?inputs:query_arg list -> db -> string -> query_output
val q_return_map :
  ?inputs:query_arg list -> db -> query_return -> query_return_map -> query -> query_output
val q_return_map_string : ?inputs:query_arg list -> db -> string -> query_output
val datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val find_datom : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
val find_datom_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
val seek_datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val seek_datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val rseek_datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val rseek_datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val index_range : db -> attr -> ?start:value -> ?stop:value -> unit -> datom list
