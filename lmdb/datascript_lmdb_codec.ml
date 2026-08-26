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
      append_byte buffer 0;
      append_int64 buffer (float_sort_bits (float_of_int value));
      Buffer.contents buffer
  | Float value ->
      let buffer = Buffer.create 16 in
      append_byte buffer 9;
      append_byte buffer 1;
      append_int64 buffer (float_sort_bits value);
      Buffer.contents buffer
  | Ref value ->
      let buffer = Buffer.create 16 in
      append_byte buffer 9;
      append_byte buffer 2;
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
      let kind, offset = read_byte bytes offset in
      let bits, offset =
        if offset + 8 > String.length bytes then invalid_arg "truncated numeric key"
        else int64_of_be (String.sub bytes offset 8), offset + 8
      in
      let float_value =
        let raw = if Int64.compare bits 0L < 0 then Int64.logxor bits 0x7fffffffffffffffL else bits in
        Int64.float_of_bits raw
      in
      (match kind with
       | 0 -> Int (int_of_float float_value)
       | 1 -> Float float_value
       | 2 -> Ref (int_of_float float_value)
       | _ -> invalid_arg "invalid numeric kind"), offset
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

let encode_datom_key index datom =
  let buffer = Buffer.create 64 in
  (match index with
   | Eavt ->
       append_int32 buffer datom.e;
       append_string buffer datom.a;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.tx
   | Aevt ->
       append_string buffer datom.a;
       append_int32 buffer datom.e;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.tx
   | Avet ->
       append_string buffer datom.a;
       append_bytes buffer (encode_value_key datom.v);
       append_int32 buffer datom.e;
       append_int32 buffer datom.tx);
  Buffer.contents buffer

let decode_datom_key index bytes =
  let e, a, v, tx =
    match index with
    | Eavt ->
        let e, offset = read_int32 bytes 0 in
        let a, offset = read_string bytes offset in
        let v, offset = decode_value_key bytes offset in
        let tx, offset = read_int32 bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing eavt key bytes";
        e, a, v, tx
    | Aevt ->
        let a, offset = read_string bytes 0 in
        let e, offset = read_int32 bytes offset in
        let v, offset = decode_value_key bytes offset in
        let tx, offset = read_int32 bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing aevt key bytes";
        e, a, v, tx
    | Avet ->
        let a, offset = read_string bytes 0 in
        let v, offset = decode_value_key bytes offset in
        let e, offset = read_int32 bytes offset in
        let tx, offset = read_int32 bytes offset in
        if offset <> String.length bytes then invalid_arg "trailing avet key bytes";
        e, a, v, tx
  in
  { e; a; v; tx; added = true }

let encode_datom_value datom =
  Marshal.to_string (datom.added, datom.v) []

let decode_datom_value bytes =
  let added, v = Marshal.from_string bytes 0 in
  { e = 0; a = ""; v; tx = 0; added }

let compare_encoded_keys index left right =
  Datascript_types.Compare.compare_datom index
    (decode_datom_key index left)
    (decode_datom_key index right)

let encode_schema schema = Marshal.to_string schema []
let decode_schema bytes = Marshal.from_string bytes 0
let encode_datoms datoms = Marshal.to_string datoms []
let decode_datoms bytes = Marshal.from_string bytes 0
