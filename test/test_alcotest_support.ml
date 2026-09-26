open Alcotest

let check_int label expected actual = check int label expected actual

let check_bool label expected actual = check bool label expected actual

let check_string_list label expected actual = check (list string) label expected actual

let check_int_list label expected actual = check (list int) label expected actual

let expect_invalid_arg f =
  match_raises "Invalid_argument" (function Invalid_argument _ -> true | _ -> false) f

let expect_invalid_arg_msg message f =
  match_raises message
    (function Invalid_argument msg when String.equal msg message -> true | _ -> false)
    f
