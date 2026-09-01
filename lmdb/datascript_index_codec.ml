open Datascript_types

let int32_be value =
  let value = Int32.of_int value in
  String.init 4 (fun index ->
    let shift = (3 - index) * 8 in
    Char.chr (Int32.to_int (Int32.shift_right_logical value shift) land 0xff))

let int32_of_be bytes =
  if String.length bytes <> 4 then invalid_arg "invalid int32 key segment";
  let byte index = Char.code bytes.[index] in
  Int32.of_int
    ((byte 0 lsl 24) lor (byte 1 lsl 16) lor (byte 2 lsl 8) lor byte 3)
  |> Int32.to_int

let int64_be_int64 value =
  String.init 8 (fun index ->
    let shift = (7 - index) * 8 in
    Char.chr (Int64.to_int (Int64.shift_right_logical value shift) land 0xff))

let int64_of_be bytes =
  if String.length bytes <> 8 then invalid_arg "invalid int64 key segment";
  let byte index = Char.code bytes.[index] in
  List.fold_left
    (fun acc index -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int (byte index)))
    0L
    [ 0; 1; 2; 3; 4; 5; 6; 7 ]

let append_bytes buffer chunk = Buffer.add_string buffer chunk

let append_int32 buffer value = append_bytes buffer (int32_be value)

let append_int64 buffer value = append_bytes buffer (int64_be_int64 value)

let float_sort_bits value =
  let bits = Int64.bits_of_float value in
  if Int64.compare bits 0L < 0 then Int64.logxor bits 0x7fffffffffffffffL else bits

let append_string buffer text =
  Buffer.add_string buffer text;
  Buffer.add_char buffer '\000'

let append_byte buffer value = Buffer.add_char buffer (Char.chr value)

let read_int32 key offset =
  if offset + 4 > String.length key then invalid_arg "truncated int32";
  int32_of_be (String.sub key offset 4), offset + 4

let read_string key offset =
  let len = String.length key in
  if offset >= len then invalid_arg "truncated string";
  let rec find_end index =
    if index >= len then invalid_arg "unterminated string"
    else if key.[index] = '\000' then index
    else find_end (index + 1)
  in
  let end_offset = find_end offset in
  String.sub key offset (end_offset - offset), end_offset + 1

let read_byte key offset =
  if offset >= String.length key then invalid_arg "truncated byte";
  Char.code key.[offset], offset + 1

let encode_keyword_like tag text =
  let namespace, name = Datascript_types.Compare.split_keyword text in
  let buffer = Buffer.create (String.length text + 16) in
  append_byte buffer tag;
  append_string buffer namespace;
  append_string buffer name;
  Buffer.contents buffer

let encode_tagged_hash tag value =
  let buffer = Buffer.create 8 in
  append_byte buffer tag;
  append_int32 buffer (Datascript_types.Compare.clojure_hasheq value);
  Buffer.contents buffer

let rec encode_value_key = function
  | Nil -> "\000"
  | Keyword value -> encode_keyword_like 1 value
  | Symbol value -> encode_keyword_like 2 value
  | Map _ as value -> encode_tagged_hash 3 value
  | Set _ as value -> encode_tagged_hash 4 value
  | List values ->
      let buffer = Buffer.create 64 in
      append_byte buffer 5;
      append_int32 buffer (List.length values);
      List.iter (fun value -> append_bytes buffer (encode_value_key value)) values;
      Buffer.contents buffer
  | Vector values ->
      let buffer = Buffer.create 64 in
      append_byte buffer 6;
      append_int32 buffer (List.length values);
      List.iter (fun value -> append_bytes buffer (encode_value_key value)) values;
      Buffer.contents buffer
  | Tuple values ->
      let buffer = Buffer.create 64 in
      append_byte buffer 7;
      append_int32 buffer (List.length values);
      List.iter
        (function
          | None -> append_byte buffer 0
          | Some value ->
              append_byte buffer 1;
              append_bytes buffer (encode_value_key value))
        values;
      Buffer.contents buffer
  | Bool false -> "\008\000"
  | Bool true -> "\008\001"
  | Int value ->
      let buffer = Buffer.create 16 in
      append_byte buffer 9;
      append_int64 buffer (float_sort_bits (float_of_int value));
      Buffer.contents buffer
  | Float value ->
      let buffer = Buffer.create 16 in
      append_byte buffer 9;
      append_int64 buffer (float_sort_bits value);
      Buffer.contents buffer
  | Ref value ->
      let buffer = Buffer.create 16 in
      append_byte buffer 9;
      append_int64 buffer (float_sort_bits (float_of_int value));
      Buffer.contents buffer
  | String value ->
      let buffer = Buffer.create (String.length value + 8) in
      append_byte buffer 10;
      append_string buffer value;
      Buffer.contents buffer
  | Regex value ->
      let buffer = Buffer.create (String.length value + 8) in
      append_byte buffer 11;
      append_string buffer value;
      Buffer.contents buffer
  | Instant value ->
      let buffer = Buffer.create 16 in
      append_byte buffer 12;
      append_int32 buffer value;
      Buffer.contents buffer
  | Uuid value ->
      let buffer = Buffer.create (String.length value + 8) in
      append_byte buffer 13;
      append_string buffer value;
      Buffer.contents buffer
  | TxRef -> "\014"
  | Ref_to value ->
      let buffer = Buffer.create 32 in
      append_byte buffer 15;
      append_int32 buffer (Hashtbl.hash value);
      Buffer.contents buffer

let rec decode_value_key bytes offset =
  let tag, offset = read_byte bytes offset in
  match tag with
  | 0 -> Nil, offset
  | 1 ->
      let namespace, offset = read_string bytes offset in
      let name, offset = read_string bytes offset in
      (if namespace = "" then Keyword name else Keyword (namespace ^ "/" ^ name)), offset
  | 2 ->
      let namespace, offset = read_string bytes offset in
      let name, offset = read_string bytes offset in
      (if namespace = "" then Symbol name else Symbol (namespace ^ "/" ^ name)), offset
  | 3 | 4 as tag ->
      let _, offset = read_int32 bytes offset in
      (if tag = 3 then Map [] else Set []), offset
  | 5 | 6 as tag ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid list length";
      let rec loop remaining offset acc =
        if remaining = 0 then
          (if tag = 5 then List (List.rev acc) else Vector (List.rev acc)), offset
        else
          let value, offset = decode_value_key bytes offset in
          loop (remaining - 1) offset (value :: acc)
      in
      loop count offset []
  | 7 ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid tuple length";
      let rec loop remaining offset acc =
        if remaining = 0 then Tuple (List.rev acc), offset
        else
          let marker, offset = read_byte bytes offset in
          let value, offset =
            match marker with
            | 0 -> None, offset
            | 1 ->
                let value, offset = decode_value_key bytes offset in
                Some value, offset
            | _ -> invalid_arg "invalid tuple slot marker"
          in
          loop (remaining - 1) offset (value :: acc)
      in
      loop count offset []
  | 8 ->
      let value, offset = read_byte bytes offset in
      (match value with 0 -> Bool false | 1 -> Bool true | _ -> invalid_arg "invalid bool key"), offset
  | 9 ->
      let bits, offset =
        if offset + 8 > String.length bytes then invalid_arg "truncated numeric key"
        else int64_of_be (String.sub bytes offset 8), offset + 8
      in
      let raw =
        if Int64.compare bits 0L < 0 then Int64.logxor bits 0x7fffffffffffffffL else bits
      in
      Float (Int64.float_of_bits raw), offset
  | 10 ->
      let value, offset = read_string bytes offset in
      String value, offset
  | 11 ->
      let value, offset = read_string bytes offset in
      Regex value, offset
  | 12 ->
      let value, offset = read_int32 bytes offset in
      Instant value, offset
  | 13 ->
      let value, offset = read_string bytes offset in
      Uuid value, offset
  | 14 -> TxRef, offset
  | 15 -> Ref_to (Entity_id 0), offset + 4
  | _ -> invalid_arg "invalid value key tag"

(* Sort keys collapse Int/Float/Ref into one numeric order. Payloads keep the
   original value constructor so EAVT/AEVT (and meta datoms) round-trip. *)
let append_len_string buffer text =
  append_int32 buffer (String.length text);
  Buffer.add_string buffer text

let read_len_string bytes offset =
  let len, offset = read_int32 bytes offset in
  if len < 0 || offset + len > String.length bytes then invalid_arg "truncated payload string";
  String.sub bytes offset len, offset + len

let rec encode_value_payload buffer value =
  match value with
  | Nil -> append_byte buffer 0
  | Keyword text ->
      let namespace, name = Datascript_types.Compare.split_keyword text in
      append_byte buffer 1;
      append_len_string buffer namespace;
      append_len_string buffer name
  | Symbol text ->
      let namespace, name = Datascript_types.Compare.split_keyword text in
      append_byte buffer 2;
      append_len_string buffer namespace;
      append_len_string buffer name
  | Map entries ->
      append_byte buffer 3;
      append_int32 buffer (List.length entries);
      List.iter
        (fun (key, value) ->
          encode_value_payload buffer key;
          encode_value_payload buffer value)
        entries
  | Set values ->
      append_byte buffer 4;
      append_int32 buffer (List.length values);
      List.iter (encode_value_payload buffer) values
  | List values ->
      append_byte buffer 5;
      append_int32 buffer (List.length values);
      List.iter (encode_value_payload buffer) values
  | Vector values ->
      append_byte buffer 6;
      append_int32 buffer (List.length values);
      List.iter (encode_value_payload buffer) values
  | Tuple values ->
      append_byte buffer 7;
      append_int32 buffer (List.length values);
      List.iter
        (function
          | None -> append_byte buffer 0
          | Some value ->
              append_byte buffer 1;
              encode_value_payload buffer value)
        values
  | Bool false ->
      append_byte buffer 8;
      append_byte buffer 0
  | Bool true ->
      append_byte buffer 8;
      append_byte buffer 1
  | Int value ->
      append_byte buffer 9;
      append_int64 buffer (Int64.of_int value)
  | Float value ->
      append_byte buffer 10;
      append_int64 buffer (Int64.bits_of_float value)
  | Ref value ->
      append_byte buffer 11;
      append_int32 buffer value
  | String text ->
      append_byte buffer 12;
      append_len_string buffer text
  | Regex text ->
      append_byte buffer 13;
      append_len_string buffer text
  | Instant value ->
      append_byte buffer 14;
      append_int64 buffer (Int64.of_int value)
  | Uuid text ->
      append_byte buffer 15;
      append_len_string buffer text
  | TxRef -> append_byte buffer 16
  | Ref_to entity_ref ->
      append_byte buffer 17;
      (match entity_ref with
       | Entity_id entity_id ->
           append_byte buffer 0;
           append_int32 buffer entity_id
       | Temp_id text ->
           append_byte buffer 1;
           append_len_string buffer text
       | CurrentTx -> append_byte buffer 2
       | Ident text ->
           append_byte buffer 3;
           append_len_string buffer text
       | Lookup_ref (attr, value) ->
           append_byte buffer 4;
           append_len_string buffer attr;
           encode_value_payload buffer value)

let rec decode_value_payload bytes offset =
  let tag, offset = read_byte bytes offset in
  match tag with
  | 0 -> Nil, offset
  | 1 | 2 as tag ->
      let namespace, offset = read_len_string bytes offset in
      let name, offset = read_len_string bytes offset in
      let text = if namespace = "" then name else namespace ^ "/" ^ name in
      (if tag = 1 then Keyword text else Symbol text), offset
  | 3 ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid map length";
      let rec loop remaining offset acc =
        if remaining = 0 then Map (List.rev acc), offset
        else
          let key, offset = decode_value_payload bytes offset in
          let value, offset = decode_value_payload bytes offset in
          loop (remaining - 1) offset ((key, value) :: acc)
      in
      loop count offset []
  | 4 | 5 | 6 as tag ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid collection length";
      let rec loop remaining offset acc =
        if remaining = 0 then
          let values = List.rev acc in
          (match tag with
           | 4 -> Set values
           | 5 -> List values
           | _ -> Vector values),
          offset
        else
          let value, offset = decode_value_payload bytes offset in
          loop (remaining - 1) offset (value :: acc)
      in
      loop count offset []
  | 7 ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid tuple length";
      let rec loop remaining offset acc =
        if remaining = 0 then Tuple (List.rev acc), offset
        else
          let marker, offset = read_byte bytes offset in
          let value, offset =
            match marker with
            | 0 -> None, offset
            | 1 ->
                let value, offset = decode_value_payload bytes offset in
                Some value, offset
            | _ -> invalid_arg "invalid tuple slot marker"
          in
          loop (remaining - 1) offset (value :: acc)
      in
      loop count offset []
  | 8 ->
      let value, offset = read_byte bytes offset in
      (match value with 0 -> Bool false | 1 -> Bool true | _ -> invalid_arg "invalid bool payload"), offset
  | 9 ->
      let raw, offset =
        if offset + 8 > String.length bytes then invalid_arg "truncated int payload"
        else int64_of_be (String.sub bytes offset 8), offset + 8
      in
      Int (Int64.to_int raw), offset
  | 10 ->
      let raw, offset =
        if offset + 8 > String.length bytes then invalid_arg "truncated float payload"
        else int64_of_be (String.sub bytes offset 8), offset + 8
      in
      Float (Int64.float_of_bits raw), offset
  | 11 ->
      let entity_id, offset = read_int32 bytes offset in
      Ref entity_id, offset
  | 12 ->
      let text, offset = read_len_string bytes offset in
      String text, offset
  | 13 ->
      let text, offset = read_len_string bytes offset in
      Regex text, offset
  | 14 ->
      let raw, offset =
        if offset + 8 > String.length bytes then invalid_arg "truncated instant payload"
        else int64_of_be (String.sub bytes offset 8), offset + 8
      in
      Instant (Int64.to_int raw), offset
  | 15 ->
      let text, offset = read_len_string bytes offset in
      Uuid text, offset
  | 16 -> TxRef, offset
  | 17 ->
      let kind, offset = read_byte bytes offset in
      (match kind with
       | 0 ->
           let entity_id, offset = read_int32 bytes offset in
           Ref_to (Entity_id entity_id), offset
       | 1 ->
           let text, offset = read_len_string bytes offset in
           Ref_to (Temp_id text), offset
       | 2 -> Ref_to CurrentTx, offset
       | 3 ->
           let text, offset = read_len_string bytes offset in
           Ref_to (Ident text), offset
       | 4 ->
           let attr, offset = read_len_string bytes offset in
           let value, offset = decode_value_payload bytes offset in
           Ref_to (Lookup_ref (attr, value)), offset
       | _ -> invalid_arg "invalid ref-to payload")
  | _ -> invalid_arg "invalid value payload tag"

let encode_index_attr_value_prefix index attr value =
  let buffer = Buffer.create 64 in
  (match index with
   | Avet ->
       append_string buffer attr;
       append_bytes buffer (encode_value_key value)
   | Aevt ->
       append_string buffer attr;
       append_int32 buffer 0;
       append_bytes buffer (encode_value_key value)
   | Eavt ->
       append_int32 buffer 0;
       append_string buffer attr;
       append_bytes buffer (encode_value_key value)
   | Tave ->
       (* Prefixed by tx elsewhere; this helper is attr|value only for Avet-like seeks. *)
       append_string buffer attr;
       append_bytes buffer (encode_value_key value));
  Buffer.contents buffer

(** TAVE key prefix [tx] or [tx | attr] for window (+ optional attr) seeks. *)
let encode_tave_tx_prefix ?attr tx =
  let buffer = Buffer.create 32 in
  append_int32 buffer tx;
  (match attr with
   | None -> ()
   | Some a -> append_string buffer a);
  Buffer.contents buffer

let append_added buffer added =
  (* dbval sort order: asserts before retracts at the same [e a v tx]. *)
  append_byte buffer (if added then 0 else 1)

let encode_datom_key index datom =
  let buffer = Buffer.create 64 in
  (match index with
   | Eavt ->
       append_int32 buffer datom.e;
       append_string buffer datom.a;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.tx;
       append_added buffer datom.added
   | Aevt ->
       append_string buffer datom.a;
       append_int32 buffer datom.e;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.tx;
       append_added buffer datom.added
   | Avet ->
       append_string buffer datom.a;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.e;
       append_int32 buffer datom.tx;
       append_added buffer datom.added
   | Tave ->
       (* tx | a | v | e | added — seek recent window + attr without full scan. *)
       append_int32 buffer datom.tx;
       append_string buffer datom.a;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.e;
       append_added buffer datom.added);
  Buffer.contents buffer

let decode_added bytes offset =
  let marker, offset = read_byte bytes offset in
  let added =
    match marker with
    | 0 -> true
    | 1 -> false
    | _ -> invalid_arg "invalid datom added key marker"
  in
  added, offset

let decode_datom_key index bytes =
  let e, a, v, tx, added =
    match index with
    | Eavt ->
        let e, offset = read_int32 bytes 0 in
        let a, offset = read_string bytes offset in
        let v, offset = decode_value_key bytes offset in
        let tx, offset = read_int32 bytes offset in
        let added, offset = decode_added bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing eavt key bytes";
        e, a, v, tx, added
    | Aevt ->
        let a, offset = read_string bytes 0 in
        let e, offset = read_int32 bytes offset in
        let v, offset = decode_value_key bytes offset in
        let tx, offset = read_int32 bytes offset in
        let added, offset = decode_added bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing aevt key bytes";
        e, a, v, tx, added
    | Avet ->
        let a, offset = read_string bytes 0 in
        let v, offset = decode_value_key bytes offset in
        let e, offset = read_int32 bytes offset in
        let tx, offset = read_int32 bytes offset in
        let added, offset = decode_added bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing avet key bytes";
        e, a, v, tx, added
    | Tave ->
        let tx, offset = read_int32 bytes 0 in
        let a, offset = read_string bytes offset in
        let v, offset = decode_value_key bytes offset in
        let e, offset = read_int32 bytes offset in
        let added, offset = decode_added bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing tave key bytes";
        e, a, v, tx, added
  in
  { e; a; v; tx; added }

let encode_datom_value datom =
  let buffer = Buffer.create 16 in
  encode_value_payload buffer datom.v;
  Buffer.contents buffer

let decode_datom_value bytes =
  let v, offset = decode_value_payload bytes 0 in
  if offset <> String.length bytes then invalid_arg "trailing datom value bytes";
  { e = 0; a = ""; v; tx = 0; added = true }

let decode_index_entry index key value =
  let datom = decode_datom_key index key in
  match index with
  | Avet | Tave -> datom
  | Eavt | Aevt ->
      let payload = decode_datom_value value in
      { datom with v = payload.v }

let encode_index_value index datom =
  match index with
  | Avet | Tave -> ""
  | Eavt | Aevt -> encode_datom_value datom

let avet_key_attr key =
  let attr, _offset = read_string key 0 in
  attr

let avet_key_value key =
  let _attr, offset = read_string key 0 in
  let value, _offset = decode_value_key key offset in
  value

let decode_avet_key_at attr key =
  let prefix_len = String.length attr + 1 in
  let v, offset = decode_value_key key prefix_len in
  let e, offset = read_int32 key offset in
  let tx, offset = read_int32 key offset in
  let added, offset = decode_added key offset in
  if offset <> String.length key then invalid_arg "trailing avet key bytes";
  { e; a = attr; v; tx; added }

let tave_key_tx key =
  let tx, _offset = read_int32 key 0 in
  tx

let compare_encoded_keys index left right =
  Datascript_types.Compare.compare_datom index
    (decode_datom_key index left)
    (decode_datom_key index right)

let cardinality_to_byte = function One -> 0 | Many -> 1

let cardinality_of_byte = function
  | 0 -> One
  | 1 -> Many
  | _ -> invalid_arg "invalid cardinality"

let unique_to_byte = function None -> 0 | Some Value -> 1 | Some Identity -> 2

let unique_of_byte = function
  | 0 -> None
  | 1 -> Some Value
  | 2 -> Some Identity
  | _ -> invalid_arg "invalid unique"

let value_type_to_byte = function
  | None -> 0
  | Some RefType -> 1
  | Some TupleType -> 2
  | Some StringType -> 3
  | Some KeywordType -> 4
  | Some NumberType -> 5
  | Some UuidType -> 6
  | Some InstantType -> 7

let value_type_of_byte = function
  | 0 -> None
  | 1 -> Some RefType
  | 2 -> Some TupleType
  | 3 -> Some StringType
  | 4 -> Some KeywordType
  | 5 -> Some NumberType
  | 6 -> Some UuidType
  | 7 -> Some InstantType
  | _ -> invalid_arg "invalid value type"

let append_string_option buffer = function
  | None -> append_byte buffer 0
  | Some text ->
      append_byte buffer 1;
      append_string buffer text

let read_string_option bytes offset =
  let marker, offset = read_byte bytes offset in
  match marker with
  | 0 -> None, offset
  | 1 ->
      let text, offset = read_string bytes offset in
      Some text, offset
  | _ -> invalid_arg "invalid string option"

let append_string_list_option buffer = function
  | None -> append_byte buffer 0
  | Some items ->
      append_byte buffer 1;
      append_int32 buffer (List.length items);
      List.iter (append_string buffer) items

let read_string_list_option bytes offset =
  let marker, offset = read_byte bytes offset in
  match marker with
  | 0 -> None, offset
  | 1 ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid string list length";
      let rec loop remaining offset acc =
        if remaining = 0 then Some (List.rev acc), offset
        else
          let item, offset = read_string bytes offset in
          loop (remaining - 1) offset (item :: acc)
      in
      loop count offset []
  | _ -> invalid_arg "invalid string list option"

let append_value_type_list_option buffer = function
  | None -> append_byte buffer 0
  | Some items ->
      append_byte buffer 1;
      append_int32 buffer (List.length items);
      List.iter (fun value_type -> append_byte buffer (value_type_to_byte (Some value_type))) items

let read_value_type_list_option bytes offset =
  let marker, offset = read_byte bytes offset in
  match marker with
  | 0 -> None, offset
  | 1 ->
      let count, offset = read_int32 bytes offset in
      if count < 0 then invalid_arg "invalid value type list length";
      let rec loop remaining offset acc =
        if remaining = 0 then Some (List.rev acc), offset
        else
          let tag, offset = read_byte bytes offset in
          (match value_type_of_byte tag with
           | Some value_type -> loop (remaining - 1) offset (value_type :: acc)
           | None -> invalid_arg "invalid tuple value type")
      in
      loop count offset []
  | _ -> invalid_arg "invalid value type list option"

let encode_schema_attr attr =
  let buffer = Buffer.create 32 in
  append_byte buffer (cardinality_to_byte attr.cardinality);
  append_byte buffer (unique_to_byte attr.unique);
  append_byte buffer (if attr.indexed then 1 else 0);
  append_byte buffer (if attr.is_component then 1 else 0);
  append_byte buffer (if attr.no_history then 1 else 0);
  append_string_option buffer attr.doc;
  append_byte buffer (value_type_to_byte attr.value_type);
  append_string_list_option buffer attr.tuple_attrs;
  append_value_type_list_option buffer attr.tuple_types;
  Buffer.contents buffer

let decode_schema_attr bytes offset =
  let cardinality, offset = read_byte bytes offset in
  let unique, offset = read_byte bytes offset in
  let indexed, offset = read_byte bytes offset in
  let is_component, offset = read_byte bytes offset in
  let no_history, offset = read_byte bytes offset in
  let doc, offset = read_string_option bytes offset in
  let value_type, offset = read_byte bytes offset in
  let tuple_attrs, offset = read_string_list_option bytes offset in
  let tuple_types, offset = read_value_type_list_option bytes offset in
  ( {
      cardinality = cardinality_of_byte cardinality
    ; unique = unique_of_byte unique
    ; indexed = indexed <> 0
    ; is_component = is_component <> 0
    ; no_history = no_history <> 0
    ; doc
    ; value_type = value_type_of_byte value_type
    ; tuple_attrs
    ; tuple_types
    }
  , offset )

let encode_schema schema =
  let buffer = Buffer.create 64 in
  append_byte buffer 1;
  append_int32 buffer (List.length schema);
  List.iter
    (fun (attr, spec) ->
      append_string buffer attr;
      append_bytes buffer (encode_schema_attr spec))
    schema;
  Buffer.contents buffer

let decode_schema bytes =
  let version, offset = read_byte bytes 0 in
  if version <> 1 then invalid_arg "unsupported schema codec version";
  let count, offset = read_int32 bytes offset in
  if count < 0 then invalid_arg "invalid schema length";
  let rec loop remaining offset acc =
    if remaining = 0 then
      if offset <> String.length bytes then invalid_arg "trailing schema bytes"
      else List.rev acc
    else
      let attr, offset = read_string bytes offset in
      let spec, offset = decode_schema_attr bytes offset in
      loop (remaining - 1) offset ((attr, spec) :: acc)
  in
  loop count offset []

let encode_datoms datoms =
  let buffer = Buffer.create 64 in
  append_byte buffer 1;
  append_int32 buffer (List.length datoms);
  List.iter
    (fun datom ->
      append_int32 buffer datom.e;
      append_string buffer datom.a;
      encode_value_payload buffer datom.v;
      append_int32 buffer datom.tx;
      append_added buffer datom.added)
    datoms;
  Buffer.contents buffer

let decode_datoms bytes =
  if String.length bytes = 0 then []
  else
    let version, offset = read_byte bytes 0 in
    if version <> 1 then invalid_arg "unsupported datoms codec version";
    let count, offset = read_int32 bytes offset in
    if count < 0 then invalid_arg "invalid datoms length";
    let rec loop remaining offset acc =
      if remaining = 0 then
        if offset <> String.length bytes then invalid_arg "trailing datoms bytes"
        else List.rev acc
      else
        let e, offset = read_int32 bytes offset in
        let a, offset = read_string bytes offset in
        let v, offset = decode_value_payload bytes offset in
        let tx, offset = read_int32 bytes offset in
        let added, offset = decode_added bytes offset in
        loop (remaining - 1) offset ({ e; a; v; tx; added } :: acc)
    in
    loop count offset []
