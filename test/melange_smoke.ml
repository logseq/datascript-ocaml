open Datascript

let fail message = raise (Failure message)

let assert_equal_string label expected actual =
  if expected <> actual then
    fail (label ^ ": expected " ^ expected ^ " but got " ^ actual)

let assert_equal_string_list label expected actual =
  if expected <> actual then fail (label ^ ": string lists did not match")

let () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  (match q_string db "[:find ?name :where [1 :name ?name]]" with
   | [ [ Result_value (String "Ivan") ] ] -> ()
   | _ -> fail "Melange query returned an unexpected result");
  assert_equal_string
    "Melange regex replace"
    "a-#-b-#"
    (Built_ins.replace_regex "a-12-b-34" "[0-9]+" "#");
  assert_equal_string_list
    "Melange regex seq"
    [ "123"; "456" ]
    (Built_ins.regex_seq "[0-9]+" "a123b456")

let () =
  let base = memory_storage () in
  let reads = ref 0 in
  let storage = { base with storage_restore = (fun address ->
    incr reads; base.storage_restore address) } in
  let schema = schema_of_edn_string "{:name {:db/index true}}" in
  let db = init_db ~schema [datom ~e:1 ~a:"name" ~v:(String "v") ()] in
  store ~storage db;
  let restored = Option.get (restore storage) in
  reads := 0;
  List.iter (fun set ->
    if Persistent_sorted_set.count set <> 1 || Persistent_sorted_set.count set <> 1 then
      failwith "restored smoke count differs")
    [restored.eavt_index;restored.aevt_index;restored.avet_index];
  if !reads <> 0 then failwith "restored smoke count loaded storage"
