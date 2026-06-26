module Json = Transit_native.Transit.Json

let failf fmt = Printf.ksprintf failwith fmt

let expect_equal label expected actual =
  if not (String.equal expected actual) then
    failf "%s: expected %S, got %S" label expected actual

let test_native_backend_decodes_logseq_storage_shape () =
  match Json.of_string "[\"^ \",\"~:schema\",[\"^ \",\"~:name\",[\"^ \",\"~:db/valueType\",\"~:db.type/string\"]]]" with
  | Json.Map
      [
        ( Json.Keyword "schema",
          Json.Map
            [
              ( Json.Keyword "name",
                Json.Map [ (Json.Keyword "db/valueType", Json.Keyword "db.type/string") ] );
            ] );
      ] -> ()
  | _ -> failwith "native melange-transit backend should decode Logseq storage Transit maps"

let test_native_backend_writes_transit_json () =
  let payload =
    Json.Map
      [
        (Json.Keyword "schema", Json.Map []);
        (Json.Keyword "max-eid", Json.Int 42);
      ]
  in
  expect_equal "native transit output" "[\"^ \",\"~:schema\",[\"^ \"],\"~:max-eid\",42]" (Json.to_string payload)

let test_native_backend_roundtrips_verbose_storage_keys () =
  let payload =
    Json.Map
      [
        ( Json.Keyword "keys",
          Json.Array
            [
              Json.Array [ Json.Int 1; Json.Keyword "block/name"; Json.String "one"; Json.Int 1 ];
              Json.Array [ Json.Int 2; Json.Keyword "block/name"; Json.String "two"; Json.Int 1 ];
            ] );
      ]
  in
  match Json.of_string (Json.to_string ~mode:Json.Verbose payload) with
  | Json.Map
      [
        ( Json.Keyword "keys",
          Json.Array
            [
              Json.Array [ Json.Int 1; Json.Keyword "block/name"; Json.String "one"; Json.Int 1 ];
              Json.Array [ Json.Int 2; Json.Keyword "block/name"; Json.String "two"; Json.Int 1 ];
            ] );
      ] -> ()
  | _ -> failwith "native melange-transit backend should roundtrip storage keys in verbose mode"

let () =
  test_native_backend_decodes_logseq_storage_shape ();
  test_native_backend_writes_transit_json ();
  test_native_backend_roundtrips_verbose_storage_keys ()
