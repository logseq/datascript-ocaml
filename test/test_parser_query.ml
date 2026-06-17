open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec search index =
    if needle_len = 0 then true
    else if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else search (index + 1)
  in
  search 0

let assert_invalid_contains label substring f =
  match f () with
  | exception Invalid_argument message when contains_substring message substring -> ()
  | exception Invalid_argument message -> failf "%s: %s" label message
  | exception exn -> failf "%s: unexpected exception %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let test_parser_query__validation () =
  [ "[:find ?e :where [?x]]", "Query for unknown vars"
  ; "[:find ?e :with ?f :where [?e]]", "Query for unknown vars"
  ; "[:find ?e ?x ?t :in ?x :where [?e]]", "Query for unknown vars"
  ; "[:find ?x ?e :with ?y ?e :where [?x ?e ?y]]", ":find and :with should not use same variables"
  ; "[:find ?e :in $ $ ?x :where [?e]]", "Vars used in :in should be distinct"
  ; "[:find ?e :in ?x $ ?x :where [?e]]", "Vars used in :in should be distinct"
  ; "[:find ?e :in $ % ?x % :where [?e]]", "Vars used in :in should be distinct"
  ; "[:find ?n :with ?e ?f ?e :where [?e ?f ?n]]", "Vars used in :with should be distinct"
  ; "[:find ?x :where [$1 ?x]]", "Where uses unknown source vars"
  ; "[:find ?x :in $1 :where [$2 ?x]]", "Where uses unknown source vars"
  ; "[:find ?e :where (rule ?e)]", "Missing rules var"
  ]
  |> List.iter (fun (query, message) ->
    assert_invalid_contains query message (fun () -> ignore (Parser.parse_query_string query)))

let () = test_parser_query__validation ()
