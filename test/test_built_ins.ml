open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_value label expected actual =
  if not (Datascript.Util.value_equal expected actual) then
    failf "%s: values did not match" label

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d but got %d" label expected actual

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
  test_aggregate_result ();
  print_endline "test_built_ins ok"
