open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal label expected actual =
  if expected <> actual then failf "%s" label

let assert_invalid label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: unexpected exception %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let test_parser_return_map__test_parse_return_map () =
  let return_map input =
    let _, parsed, _ = Parser.parse_query_return_map_string input in
    parsed
  in
  assert_equal "parse :keys" (Some (Return_keys [ "x"; "y" ])) (return_map "[:find ?a ?b :keys x y :where [?a ?b]]");
  assert_equal "parse :syms" (Some (Return_syms [ "x" ])) (return_map "[:find ?a :syms x :where [?a]]");
  assert_equal
    "parse :strs"
    (Some (Return_strs [ "x"; "y"; "z" ]))
    (return_map "[:find ?a ?b ?c :strs x y z :where [?a ?b ?c]]");
  assert_equal
    "parse tuple find specs with :keys"
    (Some (Return_keys [ "x"; "y" ]))
    (return_map "[:find [?a ?b] :keys x y :where [?a ?b]]");
  assert_invalid "reject collection find :keys" (fun () -> ignore (Parser.parse_query_return_map_string "[:find [?a ...] :keys x :where [?a]]"));
  assert_invalid "reject scalar find :keys" (fun () -> ignore (Parser.parse_query_return_map_string "[:find ?a . :keys x y :where [?a]]"));
  assert_invalid "reject multiple return maps" (fun () -> ignore (Parser.parse_query_return_map_string "[:find ?a ?b :keys x y :strs zt :where [?a ?b]]"));
  assert_invalid "reject :keys count mismatch" (fun () -> ignore (Parser.parse_query_return_map_string "[:find ?a ?b :keys x y z :where [?a ?b]]"));
  assert_invalid "reject :syms count mismatch" (fun () -> ignore (Parser.parse_query_return_map_string "[:find ?a ?b :syms x :where [?a ?b]]"));
  assert_invalid "reject :strs count mismatch" (fun () -> ignore (Parser.parse_query_return_map_string "[:find ?a ?b :strs x :where [?a ?b]]"));
  assert_invalid "reject tuple :keys count mismatch" (fun () -> ignore (Parser.parse_query_return_map_string "[:find [?a ?b] :keys x :where [?a ?b]]"))

let () = test_parser_return_map__test_parse_return_map ()
