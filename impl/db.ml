open Datascript_types

let tx0 = 0x20000000

let datom ?(tx = tx0) ?(added = true) ~e ~a ~v () = { e; a; v; tx; added }

let is_datom (_ : datom) = true

let rec list_equal_by equal left right =
  match left, right with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
    equal left right && list_equal_by equal left_rest right_rest
  | [], _ :: _ | _ :: _, [] -> false

let rec entity_ref_equal left right =
  match left, right with
  | Entity_id left, Entity_id right -> left = right
  | Temp_id left, Temp_id right -> left = right
  | CurrentTx, CurrentTx -> true
  | Ident left, Ident right -> left = right
  | Lookup_ref (left_attr, left_value), Lookup_ref (right_attr, right_value) ->
    left_attr = right_attr && value_equal left_value right_value
  | _ -> false

and value_equal left right =
  match left, right with
  | Float left, Float right ->
    (classify_float left = FP_nan && classify_float right = FP_nan) || left = right
  | List left, List right -> list_equal_by value_equal left right
  | Set left, Set right -> list_equal_by value_equal left right
  | Map left, Map right ->
    list_equal_by
      (fun (left_key, left_value) (right_key, right_value) ->
         value_equal left_key right_key && value_equal left_value right_value)
      left
      right
  | Tuple left, Tuple right ->
    list_equal_by
      (fun left right ->
         match left, right with
         | None, None -> true
         | Some left, Some right -> value_equal left right
         | None, Some _ | Some _, None -> false)
      left
      right
  | Ref_to left, Ref_to right -> entity_ref_equal left right
  | _ -> left = right

let same_fact left right = left.e = right.e && left.a = right.a && value_equal left.v right.v

let visible_datoms db index =
  let datoms =
    match index with
    | Eavt -> db.eavt_index
    | Aevt -> db.aevt_index
    | Avet -> db.avet_index
    | Vaet -> db.vaet_index
  in
  match db.filter_pred with
  | None -> datoms
  | Some pred -> List.filter pred datoms

let diff left right =
  let left_datoms = visible_datoms left Eavt in
  let right_datoms = visible_datoms right Eavt in
  ( List.filter (fun d -> not (List.exists (same_fact d) right_datoms)) left_datoms
  , List.filter (fun d -> not (List.exists (same_fact d) left_datoms)) right_datoms
  , List.filter (fun d -> List.exists (same_fact d) right_datoms) left_datoms
  )

let squuid_counter = ref 0

let squuid ?msec () =
  incr squuid_counter;
  let msec =
    match msec with
    | Some msec -> msec
    | None -> int_of_float (Unix.gettimeofday () *. 1000.0)
  in
  let seconds = msec / 1000 in
  let r1 = Random.bits () land 0xffff in
  let r2 = ((Random.bits () land 0x0fff) lor 0x4000) land 0xffff in
  let r3 = ((Random.bits () land 0x3fff) lor 0x8000) land 0xffff in
  let r4 = !squuid_counter land 0xffff in
  let r5 = Random.bits () land 0xffff in
  let r6 = Random.bits () land 0xffff in
  Uuid (Printf.sprintf "%08x-%04x-%04x-%04x-%04x%04x%04x" seconds r1 r2 r3 r4 r5 r6)

let squuid_time_millis = function
  | Uuid uuid ->
    if String.length uuid < 8 then invalid_arg "invalid squuid";
    int_of_string ("0x" ^ String.sub uuid 0 8) * 1000
  | _ -> invalid_arg "squuid_time_millis expects a uuid value"
