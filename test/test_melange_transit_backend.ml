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

let test_native_backend_converts_edn_values () =
  let edn =
    Melange_edn_native.of_edn_string
      "{:schema {:name {:db/valueType :db.type/string}}}"
  in
  match Json.of_edn edn with
  | Json.Map
      [
        ( Json.Keyword "schema",
          Json.Map
            [
              ( Json.Keyword "name",
                Json.Map [ (Json.Keyword "db/valueType", Json.Keyword "db.type/string") ] );
            ] );
      ] -> ()
  | _ -> failwith "native melange-transit backend should convert EDN maps to Transit maps"

let test_native_backend_read_cache_wraps_at_max_entries () =
  (* transit-js caches both writes and reads; both caches wrap at
     44*44 = 1936 entries, so ^XX cache codes reuse slots. Encoding
     1936+64 distinct cacheable keywords then repeating the last one
     emits ^1C — index 1999 wrapped to slot 63, which the read cache
     must resolve to the overwritten entry, not the original.
     melange-transit 0.1.0 indexed the read cache by Hashtbl.length
     without wrapping and decoded it as the stale number-63. *)
  let max_cache_entries = 44 * 44 in
  let count = max_cache_entries + 64 in
  let last = count - 1 in
  let kw i = Json.Keyword (Printf.sprintf "long.namespace.attr/number-%d" i) in
  let payload = Json.Array (List.init count kw @ [ kw last ]) in
  match Json.of_string (Json.to_string payload) with
  | Json.Array values ->
    let decode i =
      match List.nth values i with
      | Json.Keyword s -> s
      | _ -> failf "entry %d should decode to a keyword" i
    in
    List.iteri
      (fun i _ ->
        let expected = Printf.sprintf "long.namespace.attr/number-%d" i in
        let actual = decode i in
        if not (String.equal expected actual) then
          failf "entry %d: expected %S, got %S" i expected actual)
      (List.init count Fun.id);
    expect_equal "wrapped cache reference"
      (Printf.sprintf "long.namespace.attr/number-%d" last)
      (decode count)
  | _ -> failwith "expected a transit array"

let () =
  test_native_backend_decodes_logseq_storage_shape ();
  test_native_backend_writes_transit_json ();
  test_native_backend_roundtrips_verbose_storage_keys ();
  test_native_backend_converts_edn_values ();
  test_native_backend_read_cache_wraps_at_max_entries ()
