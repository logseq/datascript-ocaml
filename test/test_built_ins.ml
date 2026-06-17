open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_value label expected actual =
  if not (Datascript.Util.value_equal expected actual) then
    failf "%s: values did not match" label

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d but got %d" label expected actual

let assert_equal_bool label expected actual =
  if expected <> actual then failf "%s: expected %b but got %b" label expected actual

let assert_equal_int_option label expected actual =
  if expected <> actual then failf "%s: integer options did not match" label

let assert_equal_string_list label expected actual =
  if expected <> actual then failf "%s: string lists did not match" label

let assert_equal_string label expected actual =
  if expected <> actual then failf "%s: expected %S but got %S" label expected actual

let assert_equal_string_option label expected actual =
  if expected <> actual then failf "%s: string options did not match" label

let assert_equal_string_list_option label expected actual =
  if expected <> actual then failf "%s: string list options did not match" label

let assert_equal_result label expected actual =
  if expected <> actual then failf "%s: query results did not match" label

let assert_equal_value_option label expected actual =
  match expected, actual with
  | None, None -> ()
  | Some expected, Some actual -> assert_equal_value label expected actual
  | Some _, None -> failf "%s: expected Some value but got None" label
  | None, Some _ -> failf "%s: expected None but got Some value" label

let assert_raises_invalid_arg label f =
  match f () with
  | _ -> failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()

let test_eval_arithmetic () =
  assert_equal_value_option
    "addition preserves float results"
    (Some (Float 3.5))
    (Built_ins.eval_arithmetic AddNumbers [ Int 1; Float 2.5 ]);
  assert_equal_value_option
    "division returns an integer when the quotient is integral"
    (Some (Int 2))
    (Built_ins.eval_arithmetic DivideNumbers [ Int 4; Int 2 ]);
  assert_equal_value_option
    "division returns a float when the quotient is fractional"
    (Some (Float 2.5))
    (Built_ins.eval_arithmetic DivideNumbers [ Int 5; Int 2 ]);
  assert_equal_value_option
    "modulo follows Clojure sign semantics"
    (Some (Int 1))
    (Built_ins.eval_arithmetic ModuloNumbers [ Int (-3); Int 2 ]);
  assert_raises_invalid_arg "integer arithmetic rejects floats" (fun () ->
    ignore (Built_ins.eval_arithmetic RemainderNumbers [ Float 3.0; Int 2 ]))

let test_normalized_comparison () =
  assert_equal_int "negative comparison normalizes to -1" (-1) (Built_ins.normalized_comparison (-10));
  assert_equal_int "zero comparison normalizes to 0" 0 (Built_ins.normalized_comparison 0);
  assert_equal_int "positive comparison normalizes to 1" 1 (Built_ins.normalized_comparison 7)

let test_extremum_value () =
  assert_equal_value
    "minimum uses DataScript value ordering"
    (Keyword "a")
    (Built_ins.extremum_value MinimumValue (Keyword "b") [ Keyword "a"; Keyword "c" ]);
  assert_equal_value
    "maximum uses DataScript value ordering"
    (Int 9)
    (Built_ins.extremum_value MaximumValue (Int 3) [ Int 9; Int 4 ])

let test_string_helpers () =
  assert_equal_bool
    "string_starts_with accepts matching prefixes"
    true
    (Built_ins.string_starts_with "alphabet" "alpha");
  assert_equal_bool
    "string_starts_with rejects non-prefixes"
    false
    (Built_ins.string_starts_with "alphabet" "beta");
  assert_equal_bool
    "string_ends_with accepts matching suffixes"
    true
    (Built_ins.string_ends_with "alphabet" "bet");
  assert_equal_int_option
    "string_index_of returns the first match"
    (Some 2)
    (Built_ins.string_index_of "banana" "na");
  assert_equal_int_option
    "string_last_index_of returns the last match"
    (Some 4)
    (Built_ins.string_last_index_of "banana" "na");
  assert_equal_bool
    "string_includes returns false for missing needles"
    false
    (Built_ins.string_includes "banana" "zz");
  assert_equal_bool
    "string_is_blank accepts ascii whitespace"
    true
    (Built_ins.string_is_blank " \n\r\t\012");
  assert_equal_bool
    "string_is_blank rejects non-whitespace"
    false
    (Built_ins.string_is_blank " \tx");
  assert_equal_string_list
    "split_string keeps empty fields"
    [ "a"; ""; "b" ]
    (Built_ins.split_string "a,,b" ",");
  assert_equal_string_list
    "split_string_limited stops at the requested part count"
    [ "a"; "b,c" ]
    (Built_ins.split_string_limited "a,b,c" "," 2);
  assert_equal_string_list
    "split_lines handles lf crlf and cr separators"
    [ "a"; "b"; "c"; "d" ]
    (Built_ins.split_lines "a\nb\r\nc\rd");
  assert_equal_string
    "string_of_query_value prints scalar values for str"
    ":page/title"
    (Built_ins.string_of_query_value (Keyword "page/title"));
  assert_equal_string
    "print_query_value readably escapes strings"
    "\"a\\n\\\"b\""
    (Built_ins.print_query_value ~readably:true (String "a\n\"b"));
  assert_equal_string
    "print_query_value prints nested collections"
    "[one nil :two]"
    (Built_ins.print_query_value
       ~readably:false
       (Tuple [ Some (String "one"); None; Some (Keyword "two") ]));
  assert_equal_string_list_option
    "collection_string_values stringifies tuple nils as empty strings"
    (Some [ "a"; ""; ":b" ])
    (Built_ins.collection_string_values (Tuple [ Some (String "a"); None; Some (Keyword "b") ]));
  assert_equal_string
    "replace_string replaces all plain string matches"
    "bo no no"
    (Built_ins.replace_string "ba na na" "a" "o");
  assert_equal_string
    "replace_string first_only replaces only the first plain string match"
    "bo na na"
    (Built_ins.replace_string ~first_only:true "ba na na" "a" "o");
  assert_equal_string
    "replace_regex replaces regex matches"
    "a-#-b-#"
    (Built_ins.replace_regex "a-12-b-34" "[0-9]+" "#");
  assert_equal_string
    "escape_string replaces mapped characters"
    "a&lt;b&gt;"
    (Built_ins.escape_string "a<b>" [ String "<", String "&lt;"; String ">", String "&gt;" ]);
  assert_equal_string_option
    "regex_find returns the first regex match"
    (Some "123")
    (Built_ins.regex_find "[0-9]+" "a123b456");
  assert_equal_string_option
    "regex_matches requires a full string match"
    None
    (Built_ins.regex_matches "[0-9]+" "a123");
  assert_equal_string_list
    "regex_seq returns all matches"
    [ "123"; "456" ]
    (Built_ins.regex_seq "[0-9]+" "a123b456");
  assert_equal_string_list
    "split_regex_limited stops at the requested part count"
    [ "a"; "b-c" ]
    (Built_ins.split_regex_limited "a1b-c" "[0-9]+" 2)

let test_aggregate_result () =
  let values =
    [ Result_value (Int 1); Result_value (Int 2); Result_value (Int 2); Result_value (Float 3.5) ]
  in
  assert_equal_result "count counts all rows" (Result_value (Int 4)) (Built_ins.aggregate_result Count values);
  assert_equal_result
    "count-distinct counts unique query results"
    (Result_value (Int 3))
    (Built_ins.aggregate_result CountDistinct values);
  assert_equal_result
    "distinct returns a normalized set of scalar values"
    (Result_value (Set [ Int 1; Int 2; Float 3.5 ]))
    (Built_ins.aggregate_result Distinct values);
  assert_equal_result "sum preserves float totals" (Result_value (Float 8.5)) (Built_ins.aggregate_result Sum values);
  assert_equal_result "avg returns a float" (Result_value (Float 2.125)) (Built_ins.aggregate_result Avg values);
  assert_equal_result
    "median sorts numeric values"
    (Result_value (Float 2.0))
    (Built_ins.aggregate_result Median values);
  assert_equal_result
    "variance uses population variance"
    (Result_value (Float 0.796875))
    (Built_ins.aggregate_result Variance values);
  assert_equal_result
    "min uses aggregate result ordering"
    (Result_value (Int 1))
    (Built_ins.aggregate_result Min values);
  assert_equal_result
    "max uses aggregate result ordering"
    (Result_value (Float 3.5))
    (Built_ins.aggregate_result Max values);
  assert_equal_result
    "min n returns an ordered tuple"
    (Result_value (Tuple [ Some (Int 1); Some (Int 2) ]))
    (Built_ins.aggregate_result (MinN 2) values);
  assert_equal_result
    "max n returns an ordered tuple"
    (Result_value (Tuple [ Some (Int 2); Some (Float 3.5) ]))
    (Built_ins.aggregate_result (MaxN 2) values);
  assert_raises_invalid_arg "avg rejects empty input" (fun () ->
    ignore (Built_ins.aggregate_result Avg []));
  assert_raises_invalid_arg "dynamic aggregate amounts must be resolved before evaluation" (fun () ->
    ignore (Built_ins.aggregate_result (MinNVar "n") values))

let () =
  test_eval_arithmetic ();
  test_normalized_comparison ();
  test_extremum_value ();
  test_string_helpers ();
  test_aggregate_result ();
  print_endline "test_built_ins ok"
