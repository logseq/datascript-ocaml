(** DataScript's immutable in-memory database engine.

    The public surface is organized into small focused modules. The
    implementation [.ml] follows upstream DataScript's model; this
    interface exposes an idiomatic, type-safe, evolvable API:

    - {!module:Entity_id}, {!module:Tx_id}, {!module:Attr},
      {!module:Var}, {!module:Source}, {!module:Rule_name},
      {!module:Symbol} and {!module:Ident}: semantic wrapper types for
      the different identifier domains. Values are created with
      [Module.of_int]/[Module.of_string] and read back with
      [(x :> int)]/[(x :> string)] or [Module.to_int]/[Module.to_string],
      so unrelated domains cannot be mixed accidentally.
    - {!module:Db}, {!module:Conn}, {!module:Entity}: the abstract
      [db], [conn] and [entity] types and their operations.
    - {!module:Schema}: the abstract [schema_attr] record plus the
      {!val:Schema.spec} smart constructor, which makes the invalid
      tuple combinations of the flat record unrepresentable.
    - {!module:Tx}: transaction data constructors.
    - {!module:Pull}: the pull API.
    - {!module:Query}: [q] and friends, plus {!val:Query.v} and
      accessors over the abstract [query] type.
    - {!module:Clause}: a small clause AST — [view] decomposes a
      clause into a semantic {!clause_view}, and [call]/[pattern]/...
      build clauses through the builtin registry.
    - {!module:Storage}: durable storage through the bytes-level
      {!module-type:Storage.S} backend interface.
    - {!module:Edn}: the EDN reader.
    - {!module:Compat}: deprecated [_bang]/[is_*]/async aliases kept
      for compatibility.
    - {!module:Internal}: the lower-level implementation modules, for
      codecs, tests and ports. Not a stable public API.

    Types [db], [entity], [conn], [storage], [schema_attr], [query],
    [query_clause] and [query_input] are abstract: they are created
    and decomposed through the modules above. *)

(** {1 Primitive domains} *)

module Entity_id : sig
  (** The domain of entity identifiers (64-bit). *)

  type t = private int64

  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_int : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end


(** {1 Internal modules}

    Lower-level implementation modules exposed for the in-repo codec,
    parser and test ports. These are the full implementation
    signatures — subject to change between releases. Prefer the
    module-level API above. *)

module Internal : sig
  module PSet : module type of Persistent_sorted_set
  module Built_ins : module type of Built_ins
  module Util : module type of Util
  module Lru : module type of Lru
  module Lookup_refs : module type of Lookup_refs
  module Schema : module type of Schema
  module Schema_access : module type of Schema_access
  module Serialize : module type of Serialize
  module Conn : module type of Conn
  module Db : module type of Db
  module Entity : module type of Entity
  module Storage : module type of Storage
  module Transact : module type of Transact
  module Transact_datoms : module type of Transact_datoms
  module Db_access : module type of Db_access
  module Entity_refs : module type of Entity_refs
  module Query : module type of Query
  module Query_runtime : module type of Query_runtime
  module Query_where : module type of Query_where
  module Query_api : module type of Query_api
  module Query_eval : module type of Query_eval
  module Parser : module type of Parser
  val default_parser_context : Parser.query_context
  val default_pull_parser_context : Pull_parser.context
  val default_pull_api_context : Pull_api.context
  val default_data_readers_context : Data_readers.context
  val default_entity_context : Entity.context
  val default_storage_tail_context : Storage.tail_context
  val default_storage_restore_context : Storage.restore_context
  val empty_db :
    ?schema:Datascript_types.schema ->
    ?storage:Datascript_types.storage ->
    unit ->
    Datascript_types.db
  val init_db :
    ?schema:Datascript_types.schema ->
    ?storage:Datascript_types.storage ->
    Datascript_types.datom list ->
    Datascript_types.db
  val db_with : Datascript_types.tx_op list -> Datascript_types.db -> Datascript_types.db
  val create_conn :
    ?schema:Datascript_types.schema ->
    ?storage:Datascript_types.storage ->
    unit ->
    Conn.t
  val conn_from_datoms :
    ?schema:Datascript_types.schema ->
    ?storage:Datascript_types.storage ->
    Datascript_types.datom list ->
    Conn.t
  val conn_db : Conn.t -> Datascript_types.db
  val db_hash : Datascript_types.db -> int
  val q :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    Datascript_types.query ->
    Datascript_types.query_result list list
  val q_string :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    string ->
    Datascript_types.query_result list list
  val q_with :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    string list ->
    Datascript_types.query ->
    Datascript_types.query_result list list
  val q_with_string :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    string list ->
    string ->
    Datascript_types.query_result list list
  val q_sources :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    (string * Datascript_types.query_source) list ->
    Datascript_types.query ->
    Datascript_types.query_result list list
  val q_sources_string :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    (string * Datascript_types.query_source) list ->
    string ->
    Datascript_types.query_result list list
  val q_return :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    Datascript_types.query_return ->
    Datascript_types.query ->
    Datascript_types.query_output
  val q_return_string :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    string ->
    Datascript_types.query_output
  val q_return_map :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    Datascript_types.query_return ->
    Datascript_types.query_return_map ->
    Datascript_types.query ->
    Datascript_types.query_output
  val q_return_map_string :
    ?inputs:Datascript_types.query_arg list ->
    Datascript_types.db ->
    string ->
    Datascript_types.query_output
  val datoms :
    Datascript_types.db ->
    Datascript_types.index ->
    ?e:Datascript_types.entity_id ->
    ?a:Datascript_types.attr ->
    ?v:Datascript_types.value ->
    ?tx:Datascript_types.tx ->
    unit ->
    Datascript_types.datom Seq.t
  val entid :
    Datascript_types.db ->
    Datascript_types.attr ->
    Datascript_types.value ->
    Datascript_types.entity_id option
  val entid_ref :
    Datascript_types.db ->
    Datascript_types.entity_ref ->
    Datascript_types.entity_id option
  val schema : Datascript_types.db -> Datascript_types.schema
  val with_tx :
    ?tx_meta:Datascript_types.tx_meta ->
    Datascript_types.db ->
    Datascript_types.tx_op list ->
    Datascript_types.tx_report
  val transact :
    ?tx_meta:Datascript_types.tx_meta ->
    Datascript_types.db ->
    Datascript_types.tx_op list ->
    Datascript_types.tx_report
  val transact_conn :
    ?tx_meta:Datascript_types.tx_meta ->
    Conn.t ->
    Datascript_types.tx_op list ->
    Datascript_types.tx_report
  val pull :
    ?visitor:(Datascript_types.pull_visit -> unit) ->
    Datascript_types.db ->
    Datascript_types.pull_selector list ->
    Datascript_types.entity_ref ->
    Datascript_types.pulled_entity option
  val pull_many :
    ?visitor:(Datascript_types.pull_visit -> unit) ->
    Datascript_types.db ->
    Datascript_types.pull_selector list ->
    Datascript_types.entity_ref list ->
    Datascript_types.pulled_entity option list
  val entity :
    Datascript_types.db ->
    Datascript_types.entity_ref ->
    Datascript_types.entity option
  val parse_query : Datascript_types.query_form -> Datascript_types.query
  val parse_query_string : string -> Datascript_types.query
  val serializable : Datascript_types.db -> Datascript_types.serializable_db
  val from_serializable : Datascript_types.serializable_db -> Datascript_types.db
  val store : ?storage:Datascript_types.storage -> Datascript_types.db -> Datascript_types.db
  val store_tail : Datascript_types.storage -> Datascript_types.datom list list -> unit
  val memory_storage : unit -> Datascript_types.storage
  val file_storage : string -> Datascript_types.storage
  val storage_addresses : Datascript_types.storage -> Datascript_types.storage_address list
  val restore : Datascript_types.storage -> Datascript_types.db option
  val tail_compaction_threshold : Datascript_types.db -> int
  val tail_datom_count : Datascript_types.datom list list -> int
  val restore_root_snapshot :
    Datascript_types.storage -> Datascript_types.serializable_db option
  val restore_tail_groups : Datascript_types.storage -> Datascript_types.datom list list
  val db_with_tail : Datascript_types.db -> Datascript_types.datom list list -> Datascript_types.db
  val collect_garbage : Datascript_types.storage -> unit
  val datom :
    ?tx:Datascript_types.tx ->
    ?added:bool ->
    e:Datascript_types.entity_id ->
    a:Datascript_types.attr ->
    v:Datascript_types.value ->
    unit ->
    Datascript_types.datom
  val filter :
    Datascript_types.db ->
    (Datascript_types.db -> Datascript_types.datom -> bool) ->
    Datascript_types.db
  val is_reverse_ref : Datascript_types.attr -> bool
  val reverse_ref : Datascript_types.attr -> Datascript_types.attr
  val settings :
    Datascript_types.db -> (Datascript_types.attr * Datascript_types.value) list
  val restore_conn : Datascript_types.storage -> Conn.t option
  val conn_from_db : Datascript_types.db -> Conn.t
  module Pull_parser : module type of Pull_parser
  module Pull_api : module type of Pull_api
  module Data_readers : module type of Data_readers
  module Upsert : module type of Upsert
  module Datascript_types : module type of Datascript_types
end


module Tx_id : sig
  (** The domain of transaction ids ([datom.tx], 64-bit). *)

  type t = private int64

  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_int : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

type attr = string
(** Attribute names (["db/ident"], [":user/name"], ...). *)

type var = string
(** Query variable names, interned without their [?] sigil: ["?e"] and
    ["e"] denote the same variable. *)

type source_var = string
(** Source aliases, interned without their [$] sigil; the default
    source is the bare name ["$"]. *)

type rule_name = string
(** Rule names ([parent], [ancestor], ...). *)

type sym = string
(** Function and callable names ([str], [my-fn], ...). *)

type ident = string
(** [:db/ident] values ([":status/done"], ...). *)

type entity_id = Entity_id.t
type tx = Tx_id.t

(** [eid] is {!Entity_id.of_int64}; [txid] is {!Tx_id.of_int64}. *)

val eid : int64 -> entity_id
val txid : int64 -> tx

(** {1 Values, datoms and schema} *)

type cardinality =
  | One
  | Many

type unique =
  | Value
  | Identity

type value_type =
  | RefType
  | TupleType
  | StringType
  | KeywordType
  | NumberType
  | UuidType
  | InstantType

type value =
  | Nil
  | Int64 of int64
  | Float of float
  | String of string
  | Symbol of string
  | Bool of bool
  | Keyword of string
  | Uuid of string
  | Instant of int64
  | Regex of string
  | Ref of entity_id
  | List of value list
  | Vector of value list
  | Map of (value * value) list
  | Set of value list
  | Tuple of value option list
  | TxRef
  | Ref_to of entity_ref

and entity_ref =
  | Entity_id of entity_id
  | Temp_id of string
  | CurrentTx
  | Ident of string
  | Lookup_ref of attr * value

(** A tuple attribute spec: either a homogeneous composite of named
    attributes ([:db/tupleAttrs]) or a fixed-type tuple
    ([:db/tupleTypes]). *)

type tuple_spec =
  | Tuple_attrs of attr list
  | Tuple_types of value_type list

type value_spec =
  | Scalar_type of value_type
  | Tuple_type of tuple_spec

type schema_attr

type schema = (attr * schema_attr) list

type datom =
  { e : entity_id
  ; a : attr
  ; v : value
  ; tx : tx
  ; added : bool
  }

(** The three maintained indexes. *)

type index =
  | Eavt
  | Aevt
  | Avet

type serializable_db =
  { serializable_schema : schema
  ; serializable_datoms : datom list
  ; serializable_max_eid : entity_id
  ; serializable_max_tx : tx
  }

type storage_address = string
type storage_payload

type db

type conn

type entity

type storage

(** {1 Transactions} *)

type tx_value =
  | One_value of value
  | Many_values of value list
  | One_entity of tx_entity
  | Many_entities of tx_entity list

and tx_entity =
  { db_id : entity_ref option
  ; attrs : (attr * tx_value) list
  }

type tx_op =
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

type tx_meta = (attr * value) list

type tx_report =
  { db_before : db
  ; db_after : db
  ; tx_data : datom list
  ; tempids : (string * entity_id) list
  ; tx_meta : tx_meta
  }

module Tx : sig
  (** Constructors for transaction operations ([tx_op]). *)

  val add : entity_ref -> attr -> value -> tx_op
  val retract : entity_ref -> attr -> value -> tx_op
  val retract_attr : entity_ref -> attr -> tx_op
  val retract_entity : entity_ref -> tx_op
  val compare_and_set : entity_ref -> attr -> value option -> value -> tx_op
  val entity : tx_entity -> tx_op
  val raw_datom : datom -> tx_op
  val install_tx_fn : entity_ref -> (db -> value list -> tx_op list) -> tx_op
  val call_ident : entity_ref -> value list -> tx_op
  val call : (db -> tx_op list) -> tx_op
end

(** {1 EDN} *)

type query_form =
  | QueryFormNil
  | QueryFormBool of bool
  | QueryFormInt of int64
  | QueryFormFloat of float
  | QueryFormString of string
  | QueryFormKeyword of string
  | QueryFormSymbol of string
  | QueryFormVector of query_form list
  | QueryFormList of query_form list
  | QueryFormSet of query_form list
  | QueryFormTagged of string * query_form
  | QueryFormMap of (query_form * query_form) list

module Edn : sig
  (** The EDN reader, mirroring upstream [datascript.edn]. *)

  type t = query_form

  val read : string -> t
  val read_string : string -> t
  val attr_of_key : t -> attr
  val tx_attr_of_key : t -> attr
  val tx_op_name : t -> string
  val is_attr_key : t -> bool
  val keyword_name : t -> string
  val entity_ref : t -> entity_ref
  val to_tx_data : t -> tx_op list
  val to_schema : t -> schema
  val to_db : t -> db
end

val read_edn : string -> query_form
val parse_tx_data_string : string -> tx_op list
val schema_of_edn_string : string -> schema
val db_from_reader_string : string -> db
val attr_of_edn_key : query_form -> attr
val tx_attr_of_edn_key : query_form -> attr
val tx_op_name_of_edn_form : query_form -> string
val is_edn_attr_key : query_form -> bool
val keyword_name_of_form : query_form -> string
val entity_ref_of_edn_form : query_form -> entity_ref
val tx_data_of_edn_form : query_form -> tx_op list
val schema_of_edn_form : query_form -> schema
val db_from_reader_form : query_form -> db

(** {1 Pull} *)

type pulled_entity =
  { pulled_id : entity_id
  ; pulled_attrs : (pull_key * pulled_value) list
  }

and pull_key = value

and pulled_value =
  | Pulled_scalar of value
  | Pulled_many of pulled_value list
  | Pulled_entity of pulled_entity

type pull_visit =
  | PullVisitAttr of entity_id * attr
  | PullVisitWildcard of entity_id
  | PullVisitReverse of attr * entity_id

type pull_selector =
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

module Pull : sig
  type pattern = pull_selector list

  val parse : db -> query_form -> pattern
  val parse_string : db -> string -> pattern
  val pull :
    ?visitor:(pull_visit -> unit) -> db -> pattern -> entity_ref -> pulled_entity option
  val pull_string :
    ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref -> pulled_entity option
  val pull_many :
    ?visitor:(pull_visit -> unit) ->
    db ->
    pattern ->
    entity_ref list ->
    pulled_entity option list
  val pull_many_string :
    ?visitor:(pull_visit -> unit) ->
    db ->
    string ->
    entity_ref list ->
    pulled_entity option list
end

val parse_pull_pattern : db -> query_form -> pull_selector list
val parse_pull_pattern_string : db -> string -> pull_selector list
val pull :
  ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref -> pulled_entity option
val pull_string :
  ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref -> pulled_entity option
val pull_many :
  ?visitor:(pull_visit -> unit) ->
  db ->
  pull_selector list ->
  entity_ref list ->
  pulled_entity option list
val pull_many_string :
  ?visitor:(pull_visit -> unit) ->
  db ->
  string ->
  entity_ref list ->
  pulled_entity option list

(** {1 Queries} *)

type query_term =
  | QVar of var
  | QEntity of entity_id
  | QIdent of ident
  | QLookupRef of attr * value
  | QAttr of attr
  | QValue of value
  | QSource of source_var
  | QWildcard

type query_result =
  | Result_entity of entity_id
  | Result_attr of attr
  | Result_value of value
  | Result_db of db
  | Result_pull of pulled_entity

type query_source =
  | Db_source of db
  | Relation_source of query_result list list

type value_predicate =
  | NumberValue
  | IntegerValue
  | StringValue
  | BooleanValue
  | KeywordValue

type numeric_predicate =
  | ZeroNumber
  | PositiveNumber
  | NegativeNumber
  | EvenInteger
  | OddInteger

type comparison_predicate =
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual

type equality_predicate =
  | EqualValues
  | NotEqualValues

type arithmetic_op =
  | AddNumbers
  | SubtractNumbers
  | MultiplyNumbers
  | DivideNumbers
  | IncrementNumber
  | DecrementNumber
  | QuotientNumbers
  | RemainderNumbers
  | ModuloNumbers

type extremum_op =
  | MinimumValue
  | MaximumValue

type boolean_predicate =
  | TrueValue
  | FalseValue
  | NilValue
  | SomeValue

type query_clause

type input_binding =
  | Bind_scalar of var
  | Bind_ignore
  | Bind_collection of input_binding
  | Bind_tuple of input_binding list

(** A builtin or dynamic call: the head [call_fn] resolved through the
    builtin registry applied to [call_args]. *)

type call =
  { call_fn : sym
  ; call_args : query_term list
  }

type pattern =
  { pattern_e : query_term
  ; pattern_a : query_term
  ; pattern_v : query_term
  ; pattern_tx : query_term option
  ; pattern_op : query_term option
  }

(** The semantic shape of a clause: builtin calls appear as named
    {!Pred_view}/{!Fn_view} calls whatever internal representation
    they use. Source-qualified clauses decompose as
    [Source_view (src, inner)]. *)

type clause_view =
  | Pattern_view of pattern
  | Relation_view of query_term list
  | Pred_view of call
  | Fn_view of call * input_binding
  | Not_view of query_clause list
  | Not_join_view of var list * query_clause list
  | Or_view of query_clause list list
  | Or_join_view of var list * query_clause list list
  | Or_join_required_view of var list * var list * query_clause list list
  | Rule_view of rule_name * query_term list
  | Source_view of source_var * query_clause

module Clause : sig
  type t = query_clause

  (** [view clause] decomposes a clause into its semantic shape. *)

  val view : t -> clause_view

  (** [of_form form] parses a clause from its EDN form. *)

  val of_form : query_form -> t

  val pattern :
    ?tx:query_term ->
    ?op:query_term ->
    ?src:source_var ->
    query_term ->
    query_term ->
    query_term ->
    t

  (** [relation terms] is a relation clause [[terms ...]] — the
      EDN form [[?a ?b]]. *)

  val relation : ?src:source_var -> query_term list -> t

  (** [rule_call name args] is the rule invocation [(name args...)]. *)

  val rule_call : ?src:source_var -> rule_name -> query_term list -> t

  (** [call fn args] builds the clause for [(fn args...)]: the head is
      resolved through the builtin registry, so builtins produce the
      same clause as parsing and unknown heads become dynamic calls.
      [~binding] adds the output binding, producing a function call
      clause. *)

  val call : ?src:source_var -> ?binding:input_binding -> sym -> query_term list -> t

  (** [pred fn args] is [call fn args] restricted to predicate
      position. *)

  val pred : ?src:source_var -> sym -> query_term list -> t

  (** [fn fn args binding] is [call fn args ~binding]. *)

  val fn : ?src:source_var -> sym -> query_term list -> input_binding -> t

  val not_ : ?src:source_var -> t list -> t
  val not_join : ?src:source_var -> var list -> t list -> t
  val or_ : ?src:source_var -> t list list -> t
  val or_join : ?src:source_var -> var list -> t list list -> t
  val or_join_required :
    ?src:source_var -> required:var list -> var list -> t list list -> t
  val missing : ?src:source_var -> query_term -> query_term -> t

  (** Desugared clause shapes.  The remaining builders construct the
      specific clauses the builtin registry desugars to; [?src] wraps
      them in a source-scoped clause.  Bound output names are [var]s. *)

  val get_else :
    ?src:source_var ->
    query_term ->
    query_term ->
    query_term ->
    var ->
    t
  val get_some :
    ?src:source_var -> query_term -> query_term list -> var -> var -> t
  val get_value : ?src:source_var -> query_term -> query_term -> var -> t
  val get_default_value :
    ?src:source_var ->
    query_term ->
    query_term ->
    query_term ->
    var ->
    t
  val count_value : ?src:source_var -> query_term -> var -> t
  val empty_value : ?src:source_var -> query_term -> t
  val not_empty_value : ?src:source_var -> query_term -> t
  val contains_value : ?src:source_var -> query_term -> query_term -> t
  val value_pred : ?src:source_var -> value_predicate -> query_term -> t
  val numeric_pred : ?src:source_var -> numeric_predicate -> query_term -> t
  val boolean_pred : ?src:source_var -> boolean_predicate -> query_term -> t
  val boolean_not_pred : ?src:source_var -> query_term -> t
  val boolean_and_pred : ?src:source_var -> query_term list -> t
  val boolean_or_pred : ?src:source_var -> query_term list -> t
  val differ_pred : ?src:source_var -> query_term list -> t
  val identical_pred : ?src:source_var -> query_term -> query_term -> t
  val comparison :
    ?src:source_var ->
    comparison_predicate ->
    query_term ->
    query_term ->
    t
  val comparison_n :
    ?src:source_var -> comparison_predicate -> query_term list -> t
  val equality : ?src:source_var -> equality_predicate -> query_term list -> t
  val arithmetic :
    ?src:source_var -> arithmetic_op -> query_term list -> var -> t
  val custom_pred :
    ?src:source_var ->
    sym ->
    query_term list ->
    (query_result list -> bool) ->
    t

  (** [custom_fn name args out_vars f] binds [f] applied to the resolved
      [args] into [out_vars]. *)

  val custom_fn :
    ?src:source_var ->
    sym ->
    query_term list ->
    var list ->
    (query_result list -> query_result list option) ->
    t
end

type aggregate =
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
  | MinNVar of var
  | MaxNVar of var
  | RandNVar of var
  | SampleVar of var
  | CustomVar of var
  | Custom of (query_result list -> query_result)

type find_spec =
  | Find_var of var
  | Find_pull of var * pull_selector list
  | Find_pull_form of var * query_form
  | Find_pull_var of var * var
  | Find_pull_source of source_var * var * pull_selector list
  | Find_pull_source_form of source_var * var * query_form
  | Find_pull_source_var of source_var * var * var
  | Find_aggregate of aggregate * query_term list

(** Declaration-only [:in] elements — the shape a query declares.
    Runtime values bound at evaluation time use {!query_arg}. *)

type input_spec =
  | Spec_source of source_var
  | Spec_scalar of var
  | Spec_collection of var
  | Spec_collection_ignore
  | Spec_nested_collection of input_binding
  | Spec_tuple of var list
  | Spec_relation of var list
  | Spec_nested_tuple of input_binding list
  | Spec_nested_relation of input_binding list
  | Spec_rules
  | Spec_ignore

type query_rule =
  { rule_name : rule_name
  ; rule_params : var list
  ; rule_body : query_clause list
  }

(** A runtime argument bound to an [:in] spec when evaluating a
    query. *)

type query_arg =
  | Arg_scalar of query_result
  | Arg_entity_ref of entity_ref
  | Arg_collection of query_result list
  | Arg_tuple of query_result list
  | Arg_relation of query_result list list
  | Arg_predicate of (query_result list -> bool)
  | Arg_function of (query_result list -> query_result list option)
  | Arg_aggregate of (query_result list -> query_result)
  | Arg_rules of query_rule list

type query

type query_return =
  | Return_relation
  | Return_collection
  | Return_tuple
  | Return_scalar

type query_return_map =
  | Return_keys of string list
  | Return_syms of string list
  | Return_strs of string list

type query_output =
  | Query_relation of query_result list list
  | Query_collection of query_result list
  | Query_tuple of query_result list option
  | Query_scalar of query_result option
  | Query_relation_maps of (value * query_result) list list
  | Query_tuple_map of (value * query_result) list option

(** Bound runtime inputs produced by input application; kept abstract
    and reachable through {!Internal}. *)

type query_input

module Query : sig
  type t = query

  (** [v find where] builds a query programmatically and
      validates it exactly like the EDN parser. *)

  val v :
    ?in_:input_spec list ->
    ?with_:var list ->
    ?rules:query_rule list ->
    find_spec list ->
    query_clause list ->
    t

  (** Read-only accessors. *)

  val find : t -> find_spec list
  val where : t -> query_clause list
  val inputs : t -> input_spec list
  val with_ : t -> var list
  val rules : t -> query_rule list

  val of_form : query_form -> t
  val of_string : string -> t
  val q : ?inputs:query_arg list -> db -> t -> query_result list list
  val q_string : ?inputs:query_arg list -> db -> string -> query_result list list
  val q_with : ?inputs:query_arg list -> db -> string list -> t -> query_result list list
  val q_with_string :
    ?inputs:query_arg list -> db -> string list -> string -> query_result list list
  val q_sources :
    ?inputs:query_arg list ->
    db ->
    (string * query_source) list ->
    t ->
    query_result list list
  val q_sources_string :
    ?inputs:query_arg list ->
    db ->
    (string * query_source) list ->
    string ->
    query_result list list
  val q_return : ?inputs:query_arg list -> db -> query_return -> t -> query_output
  val q_return_string : ?inputs:query_arg list -> db -> string -> query_output
  val q_return_map :
    ?inputs:query_arg list -> db -> query_return -> query_return_map -> t -> query_output
  val q_return_map_string : ?inputs:query_arg list -> db -> string -> query_output
end

val parse_query : query_form -> query
val parse_query_string : string -> query

(** [parse_rules_string input] parses an EDN rules section ([[(name ?v) clause ...]]) *)

val parse_rules_string : string -> query_rule list
val parse_query_return : query_form -> query_return * query
val parse_query_return_string : string -> query_return * query
val parse_query_return_map : query_form -> query_return * query_return_map option * query
val parse_query_return_map_string :
  string -> query_return * query_return_map option * query
val parse_query_return_map_string_with_pull_context :
  ?default_pull_db:db ->
  ?pull_db_for_source:(string -> db) ->
  string ->
  query_return * query_return_map option * query
val parse_binding : query_form -> input_binding
val parse_in : query_form -> input_spec list
val parse_with : query_form -> string list
val parse_find : query_form -> query_return * find_spec list

val q : ?inputs:query_arg list -> db -> query -> query_result list list
val q_string : ?inputs:query_arg list -> db -> string -> query_result list list
val q_with : ?inputs:query_arg list -> db -> string list -> query -> query_result list list
val q_with_string :
  ?inputs:query_arg list -> db -> string list -> string -> query_result list list
val q_sources :
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  query ->
  query_result list list
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

(** {1 Database values} *)

module Db : sig
  type t = db

  val empty : ?schema:schema -> ?storage:storage -> unit -> db
  val init : ?schema:schema -> ?storage:storage -> datom list -> db
  val tx0 : tx
  val datom : ?tx:tx -> ?added:bool -> entity_id -> attr -> value -> datom
  val serializable : db -> serializable_db
  val from_serializable : serializable_db -> db
  val db_from_reader_string : string -> db
  val filter : db -> (db -> datom -> bool) -> db
  val unfiltered : db -> db
  val with_ : tx_op list -> db -> db
  val with_string : string -> db -> db
  val with_tx : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
  val with_tx_string : ?tx_meta:tx_meta -> db -> string -> tx_report
  val schema : db -> schema
  val with_schema : db -> schema -> db
  val datoms :
    ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
  val fold_datoms :
    ?e:entity_id ->
    ?a:attr ->
    ?v:value ->
    ?tx:tx ->
    ('acc -> datom -> 'acc) ->
    'acc ->
    db ->
    index ->
    'acc
  val datoms_ref :
    ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
  val find_datom :
    ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom option
  val find_datom_ref :
    ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom option
  val seek_datoms :
    ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
  val seek_datoms_ref :
    ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
  val rseek_datoms :
    ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
  val rseek_datoms_ref :
    ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
  val index_range : ?start:value -> ?stop:value -> db -> attr -> datom Seq.t
  val entid : db -> attr -> value -> entity_id option
  val entid_ref : db -> entity_ref -> entity_id option
  val hash : db -> int
  val hash_cache_size : unit -> int
  val diff : db -> db -> datom list * datom list * datom list
  val squuid : ?msec:int64 -> unit -> value
  val squuid_time_millis : value -> int64
  val is_unique : db -> attr -> bool
  val is_unique_identity : db -> attr -> bool
  val is_indexed : db -> attr -> bool
  val is_component : db -> attr -> bool
  val is_ref : db -> attr -> bool
  val is_tuple : db -> attr -> bool
  val tuple_attrs : db -> attr -> attr list option
  val reverse_ref : attr -> attr
  val is_datom : datom -> bool
  val value_equal : value -> value -> bool
  val same_fact : datom -> datom -> bool
end

module Conn : sig
  type t = conn

  val create : ?schema:schema -> ?storage:storage -> unit -> conn
  val from_db : db -> conn
  val from_datoms : ?schema:schema -> ?storage:storage -> datom list -> conn
  val restore : storage -> conn option
  val db : conn -> db
  val update_db : conn -> (db -> db) -> unit
  val storage_tail : conn -> datom list list
  val listen : conn -> string -> (tx_report -> unit) -> string
  val listen_auto : conn -> (tx_report -> unit) -> string
  val unlisten : conn -> string -> unit
  val reset : ?tx_meta:tx_meta -> conn -> db -> db
  val reset_schema : conn -> schema -> db
  val apply_report : conn -> tx_report -> tx_report
  val transact : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
  val transact_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
  val transact_async : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
end

module Entity : sig
  type t = entity

  val id : entity -> entity_id
  val of_ref : db -> entity_ref -> entity option
  val attr : entity -> attr -> tx_value option
  val attr_raw : entity -> attr -> tx_value option
  val attrs : entity -> (attr * tx_value) list
  val db : entity -> db
  val equal : entity -> entity -> bool
  val hash : entity -> int
  val touch : entity -> entity
  val is_entity : entity -> bool
end

module Schema : sig
  type t = schema_attr

  (** [spec] builds a [schema_attr]. Tuple attributes and tuple types
      are expressed through {!tuple_spec}, so the invalid combinations
      of the flat record cannot be constructed: a tuple spec implies
      [valueType TupleType], and [tupleAttrs] and [tupleTypes] are
      mutually exclusive. *)

  val spec :
    ?indexed:bool ->
    ?value_type:value_type ->
    ?cardinality:cardinality ->
    ?unique:unique ->
    ?is_component:bool ->
    ?no_history:bool ->
    ?doc:string ->
    ?tuple:tuple_spec ->
    unit ->
    t

  val validate : schema -> schema
  val fields : attr list
  val attr_by_name : schema -> attr -> schema_attr option
  val cardinality : t -> cardinality
  val unique : t -> unique option
  val indexed : t -> bool
  val is_component : t -> bool
  val no_history : t -> bool
  val doc : t -> string option
  val value_type : t -> value_type option
  val tuple_attrs : t -> attr list option
  val tuple_types : t -> value_type list option
  val tuple : t -> tuple_spec option
  val is_ref : schema -> attr -> bool
  val is_tuple : schema_attr option -> bool
  val is_avet_accessible : schema -> attr -> bool
  val has_no_history : schema -> attr -> bool
  val folded_datoms : int ref
  val split_namespaced : attr -> string option * string
  val join_namespaced : string option -> string -> attr
  val is_reverse_ref : attr -> bool
  val reverse_ref : attr -> attr
end

module Storage : sig
  (** Durable storage backends.

      A [storage] is built over a small bytes-level backend (module
      {!module-type:S}) that persists opaque buffers keyed by
      addresses; the library owns serialization. For codecs that
      interoperate with ClojureScript DataScript's persisted layout
      (SQLite, Melange backends), see {!Internal.Storage}. *)

  type t = storage

  module type S = sig
    type t

    val write : t -> (storage_address * string) list -> unit
    val read : t -> storage_address list -> (storage_address * string) list
    val list : t -> storage_address list
    val delete : t -> storage_address list -> unit
  end

  val make : (module S with type t = 's) -> 's -> t
  val memory : unit -> t
  val file : string -> t
  val root_address : storage_address
  val tail_address : storage_address
  val store : ?storage:t -> db -> db
  val store_tail : t -> datom list list -> unit
  val tail_compaction_threshold : db -> int
  val tail_datom_count : datom list list -> int
  val restore_root_snapshot : t -> serializable_db option
  val restore_tail_groups : t -> datom list list
  val db_with_tail : db -> datom list list -> db
  val restore : t -> db option
  val addresses : db list -> storage_address list
  val of_db : db -> t option
  val settings : db -> (attr * value) list
  val collect_garbage : t -> unit
end

(** {1 Database lifecycle} *)

val tx0 : tx
val datom : ?tx:tx -> ?added:bool -> entity_id -> attr -> value -> datom
val empty_db : ?schema:schema -> ?storage:storage -> unit -> db
val empty : db -> db
val init_db : ?schema:schema -> ?storage:storage -> datom list -> db
val filter : db -> (db -> datom -> bool) -> db
val unfiltered_db : db -> db
val serializable : db -> serializable_db
val from_serializable : serializable_db -> db
val memory_storage : unit -> storage
val file_storage : string -> storage
val store : ?storage:storage -> db -> db
val store_tail : storage -> datom list list -> unit
val restore : storage -> db option
val db_with_tail : db -> datom list list -> db
val storage : db -> storage option
val addresses : db list -> storage_address list
val settings : db -> (attr * value) list
val storage_addresses : storage -> storage_address list
val collect_garbage : storage -> unit
val db_hash : db -> int
val db_hash_cache_size : unit -> int
val diff : db -> db -> datom list * datom list * datom list
val squuid : ?msec:int64 -> unit -> value
val squuid_time_millis : value -> int64

(** {1 Connections and transactions} *)

val create_conn : ?schema:schema -> ?storage:storage -> unit -> conn
val conn_from_db : db -> conn
val conn_from_datoms : ?schema:schema -> ?storage:storage -> datom list -> conn
val restore_conn : storage -> conn option
val conn_db : conn -> db
val db : conn -> db
val listen : conn -> string -> (tx_report -> unit) -> string
val listen_auto : conn -> (tx_report -> unit) -> string
val unlisten : conn -> string -> unit
val reset_conn : ?tx_meta:tx_meta -> conn -> db -> db
val reset_schema : conn -> schema -> db
val schema : db -> schema
val with_schema : db -> schema -> db
val db_with : tx_op list -> db -> db
val db_with_string : string -> db -> db
val transact : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
val transact_string : ?tx_meta:tx_meta -> db -> string -> tx_report
val with_tx : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
val with_tx_string : ?tx_meta:tx_meta -> db -> string -> tx_report
val transact_conn : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_conn_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val apply_report : conn -> tx_report -> tx_report
val tempid : ?part:string -> ?value:entity_id -> unit -> entity_ref
val resolve_tempid : ?db:db -> (string * entity_id) list -> string -> entity_id option

(** {1 Entities} *)

val entity : db -> entity_ref -> entity option
val entity_attr : entity -> attr -> tx_value option
val entity_attrs : entity -> (attr * tx_value) list
val entity_db : entity -> db
val entity_equal : entity -> entity -> bool
val entity_hash : entity -> int
val touch : entity -> entity
val entid : db -> attr -> value -> entity_id option
val entid_ref : db -> entity_ref -> entity_id option
val is_reverse_ref : attr -> bool
val reverse_ref : attr -> attr

(** {1 Datoms} *)

val datoms :
  ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
val fold_datoms :
  ?e:entity_id ->
  ?a:attr ->
  ?v:value ->
  ?tx:tx ->
  ('acc -> datom -> 'acc) ->
  'acc ->
  db ->
  index ->
  'acc
val datoms_ref :
  ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
val find_datom :
  ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom option
val find_datom_ref :
  ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom option
val seek_datoms :
  ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
val seek_datoms_ref :
  ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
val rseek_datoms :
  ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
val rseek_datoms_ref :
  ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> db -> index -> datom Seq.t
val index_range : ?start:value -> ?stop:value -> db -> attr -> datom Seq.t

(** {1 Compatibility aliases}

    Deprecated entry points kept for compatibility; new code should
    use the module-level API above. *)

module Compat : sig
  val is_datom : datom -> bool
  val is_db : db -> bool
  val is_conn : conn -> bool
  val is_entity : entity -> bool
  val is_filtered : db -> bool
  val listen_bang : conn -> string -> (tx_report -> unit) -> string
  val listen_bang_auto : conn -> (tx_report -> unit) -> string
  val unlisten_bang : conn -> string -> unit
  val reset_conn_bang : ?tx_meta:tx_meta -> conn -> db -> db
  val reset_schema_bang : conn -> schema -> db
  val transact_bang : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
  val transact_bang_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
  val transact_async : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
  val transact_async_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
end

(** {1 Boundary coercions}

    The public types share their runtime representation with the
    implementation types; these identity coercions move values across the
    boundary without copying. They exist so the in-repo cljs-interop codecs
    (SQLite, Transit) and test ports can pass implementation-level values
    into the public API and back. *)

module Internal_convert : sig
  val internalize_db : db -> Internal.Datascript_types.db
  val externalize_db : Internal.Datascript_types.db -> db
  val internalize_storage : storage -> Internal.Datascript_types.storage
  val externalize_storage : Internal.Datascript_types.storage -> storage
  val internalize_entity : entity -> Internal.Datascript_types.entity
  val externalize_entity : Internal.Datascript_types.entity -> entity
  val internalize_conn : conn -> Internal.Conn.t
  val externalize_conn : Internal.Conn.t -> conn
  val internalize_query : query -> Internal.Datascript_types.query
  val externalize_query : Internal.Datascript_types.query -> query
  val internalize_query_input : query_input -> Internal.Datascript_types.query_input
  val externalize_query_input : Internal.Datascript_types.query_input -> query_input
  val internalize_schema_attr : schema_attr -> Internal.Datascript_types.schema_attr
  val externalize_schema_attr : Internal.Datascript_types.schema_attr -> schema_attr
  val internalize_value : value -> Internal.Datascript_types.value
  val externalize_value : Internal.Datascript_types.value -> value
  val internalize_datom : datom -> Internal.Datascript_types.datom
  val externalize_datom : Internal.Datascript_types.datom -> datom
  val internalize_entity_ref : entity_ref -> Internal.Datascript_types.entity_ref
  val externalize_entity_ref : Internal.Datascript_types.entity_ref -> entity_ref
  val internalize_entity_id : entity_id -> Internal.Datascript_types.entity_id
  val externalize_entity_id : Internal.Datascript_types.entity_id -> entity_id
  val internalize_tx : tx -> Internal.Datascript_types.tx
  val externalize_tx : Internal.Datascript_types.tx -> tx
  val internalize_tx_op : tx_op -> Internal.Datascript_types.tx_op
  val externalize_tx_op : Internal.Datascript_types.tx_op -> tx_op
  val internalize_query_result : query_result -> Internal.Datascript_types.query_result
  val externalize_query_result : Internal.Datascript_types.query_result -> query_result
  val internalize_query_rule : query_rule -> Internal.Datascript_types.query_rule
  val externalize_query_rule : Internal.Datascript_types.query_rule -> query_rule
  val internalize_query_form : query_form -> Internal.Datascript_types.query_form
  val externalize_query_form : Internal.Datascript_types.query_form -> query_form
  val internalize_input_binding : input_binding -> Internal.Datascript_types.input_binding
  val externalize_input_binding : Internal.Datascript_types.input_binding -> input_binding
  val internalize_query_output : query_output -> Internal.Datascript_types.query_output
  val externalize_query_output : Internal.Datascript_types.query_output -> query_output
  val internalize_tx_report : tx_report -> Internal.Datascript_types.tx_report
  val externalize_tx_report : Internal.Datascript_types.tx_report -> tx_report
end
