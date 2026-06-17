open Datascript_types

type context =
  { is_unique : db -> attr -> bool
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; visible_datoms : db -> datom list
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

let entity_id_in_datoms ?(strict_missing = false) context db datoms attr value =
  if not (context.is_unique db attr) then
    invalid_arg (non_unique_message context attr value);
  match context.entid_in_datoms db datoms attr value with
  | Some entity_id -> Some entity_id
  | None ->
    if strict_missing then
      invalid_arg (unresolved_message context attr value)
    else
      None

let entity_id ?strict_missing context db attr value =
  entity_id_in_datoms ?strict_missing context db (context.visible_datoms db) attr value
