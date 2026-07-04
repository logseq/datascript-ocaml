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
