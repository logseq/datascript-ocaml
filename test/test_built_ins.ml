open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_value label expected actual =
  if not (Datascript.Util.value_equal expected actual) then
    failf "%s: values did not match" label

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d but got %d" label expected actual

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

let () =
  test_eval_arithmetic ();
  test_normalized_comparison ();
  test_extremum_value ();
  print_endline "test_built_ins ok"
