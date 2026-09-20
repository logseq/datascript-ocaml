type error = { path : string; message : string }

exception Invalid_data of error

val error_message : error -> string
val or_raise : ('a, error) result -> 'a
val of_edn : Lg_edn_backend.t -> (Datascript.query_form, error) result
val of_edn_exn : Lg_edn_backend.t -> Datascript.query_form
val read : string -> Datascript.query_form
val schema : Datascript.query_form -> Datascript.schema
val schema_spec : Datascript.query_form -> Datascript.schema_attr
val tx : Datascript.query_form -> Datascript.tx_op list
val transact : Datascript.conn -> Datascript.query_form -> Datascript.tx_report

val entity :
  Datascript.entity_ref option ->
  (Datascript.attr * Datascript.tx_value) list ->
  Datascript.tx_op

module Codec : sig
  type 'a t

  val string : string t
  val int : int t
  val float : float t
  val bool : bool t
  val keyword : string t
  val entity_id : int t
  val value : Datascript.value t
  val agree : 'a t -> 'a t -> unit
  val encode : 'a t -> 'a -> Datascript.value
  val decode : 'a t -> Datascript.value -> ('a, error) result
end

val read_one :
  'a Codec.t -> string -> Datascript.entity -> ('a option, error) result

module Attribute : sig
  type 'a t

  val make : string -> 'a Codec.t -> Datascript.schema_attr -> 'a t
  val codec : 'a t -> 'a Codec.t
  val codec_for : string -> 'a t -> 'a Codec.t
  val name : 'a t -> string
  val schema : 'a t -> Datascript.attr * Datascript.schema_attr
  val entry : 'a t -> 'a -> Datascript.attr * Datascript.tx_value
  val entries : 'a t -> 'a list -> Datascript.attr * Datascript.tx_value
  val read_one : 'a t -> Datascript.entity -> ('a option, error) result
  val read_many : 'a t -> Datascript.entity -> ('a list, error) result
end

module Projection : sig
  type 'a t

  val column : int -> 'a Codec.t -> 'a t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val run : 'a t -> Datascript.query_result list -> ('a, error) result
end

type 'a query

val prepare_query : 'a Projection.t -> Datascript.query_form -> 'a query
val input : 'a Codec.t -> 'a -> Datascript.query_arg

val run_query :
  'a query ->
  Datascript.db ->
  Datascript.query_arg list ->
  ('a list, error) result

module Pull : sig
  type 'a t

  val field : 'a Attribute.t -> 'a option t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

val pull :
  Datascript.db ->
  Datascript.pull_selector list ->
  Datascript.entity_ref ->
  'a Pull.t ->
  ('a option, error) result

val pull_form :
  Datascript.db ->
  Datascript.query_form ->
  Datascript.entity_ref ->
  'a Pull.t ->
  ('a option, error) result
