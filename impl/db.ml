open Datascript_types

let tx0 = 0x20000000

let datom ?(tx = tx0) ?(added = true) ~e ~a ~v () = { e; a; v; tx; added }

let is_datom (_ : datom) = true

let value_equal = Util.value_equal

let same_fact left right = left.e = right.e && left.a = right.a && value_equal left.v right.v

type index_context =
  { is_avet_accessible : db -> attr -> bool
  ; resolve_entity_ref : db -> entity_ref -> entity_id
  ; resolve_value_for_optional_attr : db -> attr option -> value -> value
  ; resolve_value_for_attr : db -> attr -> value -> value
  ; compare_value : value -> value -> int
  ; first_nonzero : int list -> int
  }

let hash_cache : (int, int) Hashtbl.t = Hashtbl.create 128

let hash db =
  match Hashtbl.find_opt hash_cache db.db_uid with
  | Some hash -> hash
  | None ->
    let hash =
      Hashtbl.hash
        ( db.schema
        , db.datoms
        , db.history_datoms
        , db.historical
        , db.max_eid
        , db.max_tx
        )
    in
    Hashtbl.replace hash_cache db.db_uid hash;
    hash

let hash_cache_size () = Hashtbl.length hash_cache

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

let matches maybe expected = Option.fold ~none:true ~some:(fun actual -> actual = expected) maybe

let matches_value maybe expected =
  Option.fold ~none:true ~some:(fun actual -> value_equal actual expected) maybe

let indexed_attr_required_message attr =
  "Attribute :" ^ attr ^ " should be marked as :db/index true"

let validate_index_access context db index attr =
  match index, attr with
  | Avet, Some attr when not (context.is_avet_accessible db attr) ->
    invalid_arg (indexed_attr_required_message attr)
  | _ -> ()

let resolved_entity_ref_option context db = Option.map (context.resolve_entity_ref db)

let resolved_value_option_for_optional_attr context db attr =
  Option.map (context.resolve_value_for_optional_attr db attr)

let datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  visible_datoms db index
  |> List.filter (fun d -> matches e d.e && matches a d.a && matches_value v d.v && matches tx d.tx)

let datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  datoms context db index ?e ?a ?v ?tx ()

let find_datom context db index ?e ?a ?v ?tx () =
  match datoms context db index ?e ?a ?v ?tx () with
  | first :: _ -> Some first
  | [] -> None

let find_datom_ref context db index ?e ?a ?v ?tx () =
  match datoms_ref context db index ?e ?a ?v ?tx () with
  | first :: _ -> Some first
  | [] -> None

let compare_optional actual = function
  | Some expected -> compare actual expected
  | None -> 0

let compare_optional_with compare_item actual = function
  | Some expected -> compare_item actual expected
  | None -> 0

let compare_datom_to_bound context index d e a v tx =
  match index with
  | Eavt ->
    context.first_nonzero
      [ compare_optional d.e e
      ; compare_optional d.a a
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.tx tx
      ]
  | Aevt ->
    context.first_nonzero
      [ compare_optional d.a a
      ; compare_optional d.e e
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.tx tx
      ]
  | Avet ->
    context.first_nonzero
      [ compare_optional d.a a
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.e e
      ; compare_optional d.tx tx
      ]
  | Vaet ->
    context.first_nonzero
      [ compare_optional_with context.compare_value d.v v
      ; compare_optional d.a a
      ; compare_optional d.e e
      ; compare_optional d.tx tx
      ]

let seek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  datoms context db index ()
  |> List.filter (fun d -> compare_datom_to_bound context index d e a v tx >= 0)

let seek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  seek_datoms context db index ?e ?a ?v ?tx ()

let rseek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  datoms context db index ()
  |> List.filter (fun d -> compare_datom_to_bound context index d e a v tx <= 0)
  |> List.rev

let rseek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  rseek_datoms context db index ?e ?a ?v ?tx ()

let index_range context db attr ?start ?stop () =
  if not (context.is_avet_accessible db attr) then
    invalid_arg (indexed_attr_required_message attr);
  let start = Option.map (context.resolve_value_for_attr db attr) start in
  let stop = Option.map (context.resolve_value_for_attr db attr) stop in
  datoms context db Avet ~a:attr ()
  |> List.filter (fun d ->
    Option.fold ~none:true ~some:(fun start -> context.compare_value d.v start >= 0) start
    && Option.fold ~none:true ~some:(fun stop -> context.compare_value d.v stop <= 0) stop)

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
