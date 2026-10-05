open Datascript_types

type context =
  { is_unique : db -> attr -> bool
  ; entid : db -> attr -> value -> entity_id option
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; value_to_string : value -> string
  }

let unresolved_message context attr value =
  "Nothing found for entity id [:" ^ attr ^ " " ^ context.value_to_string value ^ "]"

let non_unique_message context attr value =
  "Lookup ref attribute should be marked as :db/unique: [:"
  ^ attr
  ^ " "
  ^ context.value_to_string value
  ^ "]"

let entity_id_of_resolution ?(strict_missing = false) context db attr value resolved =
  if not (context.is_unique db attr) then
    invalid_arg (non_unique_message context attr value);
  match resolved with
  | Some entity_id -> Some entity_id
  | None ->
    if strict_missing then
      invalid_arg (unresolved_message context attr value)
    else
      None

let entity_id_in_datoms ?strict_missing context db datoms attr value =
  entity_id_of_resolution
    ?strict_missing
    context
    db
    attr
    value
    (context.entid_in_datoms db datoms attr value)

let entity_id ?strict_missing context db attr value =
  entity_id_of_resolution ?strict_missing context db attr value (context.entid db attr value)
