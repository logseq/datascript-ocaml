open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_query label expected actual =
  if expected <> actual then failf "%s: unexpected query result" label

let test_data_readers__test_db_reader () =
  let db =
    Data_readers.db_from_reader_string
      "#datascript/DB {:schema {:email {:db/unique :db.unique/identity}
                                :friend {:db/valueType :db.type/ref}}
                       :datoms [[1 :email \"ivan@example.com\"]
                                [2 :email \"oleg@example.com\"]
                                [1 :friend 2]]}"
  in
  assert_equal_query
    "data reader restores DB schema and ref datoms"
    [ [ Result_value (String "ivan@example.com"); Result_value (String "oleg@example.com") ] ]
    (q_string db "[:find ?email ?friend-email :where [?e :email ?email] [?e :friend ?f] [?f :email ?friend-email]]")

let test_data_readers__test_datom_reader_in_tx_data () =
  let db =
    empty_db ()
    |> db_with (Data_readers.tx_data_of_edn_form (read_edn "[#datascript/Datom [1 :name \"Ivan\" 536870913 true]]"))
  in
  assert_equal_query
    "data reader turns tagged datoms into transaction data"
    [ [ Result_value (String "Ivan") ] ]
    (q_string db "[:find ?name :where [1 :name ?name]]")

let () =
  test_data_readers__test_db_reader ();
  test_data_readers__test_datom_reader_in_tx_data ()
