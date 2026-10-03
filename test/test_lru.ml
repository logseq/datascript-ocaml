open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let test_lru__test_lru () =
  let l0 = Internal.Lru.create 2 in
  let l1 = Internal.Lru.assoc "a" 1 l0 in
  let l2 = Internal.Lru.assoc "b" 2 l1 in
  let l3 = Internal.Lru.assoc "c" 3 l2 in
  let l4 = Internal.Lru.assoc "b" 4 l3 in
  let l5 = Internal.Lru.assoc "d" 5 l4 in
  if Internal.Lru.find "a" l0 <> None then failwith "empty LRU should not contain a";
  if Internal.Lru.find "a" l1 <> Some 1 then failwith "LRU should store inserted values";
  if Internal.Lru.find "a" l2 <> Some 1 then failwith "LRU should keep values under limit";
  if Internal.Lru.find "b" l2 <> Some 2 then failwith "LRU should keep the second inserted value";
  if Internal.Lru.find "a" l3 <> None then failwith "LRU should evict the oldest value on overflow";
  if Internal.Lru.find "b" l3 <> Some 2 then failwith "LRU should retain newer values";
  if Internal.Lru.find "c" l3 <> Some 3 then failwith "LRU should retain the newest value";
  if Internal.Lru.find "b" l4 <> Some 2 then failwith "LRU reassoc should update recency without replacing value";
  if Internal.Lru.find "c" l4 <> Some 3 then failwith "LRU reassoc should not evict under limit";
  if Internal.Lru.find "b" l5 <> Some 2 then failwith "LRU reassoc should keep b newer than c";
  if Internal.Lru.find "c" l5 <> None then failwith "LRU should evict the oldest value after reassoc";
  if Internal.Lru.find "d" l5 <> Some 5 then failwith "LRU should store the newest inserted value"

let test_lru__test_cache () =
  let cache = Internal.Lru.cache 2 in
  let a_calls = ref 0 in
  let b_calls = ref 0 in
  let c_calls = ref 0 in
  let compute counter value () =
    incr counter;
    value
  in
  if Internal.Lru.cache_get cache "a" (compute a_calls 1) <> 1 then failwith "cache should compute first miss";
  if Internal.Lru.cache_get cache "b" (compute b_calls 2) <> 2 then failwith "cache should compute second miss";
  if Internal.Lru.cache_get cache "a" (compute a_calls 11) <> 1 then failwith "cache should reuse cached values";
  if Internal.Lru.cache_get cache "c" (compute c_calls 3) <> 3 then failwith "cache should compute new miss";
  if Internal.Lru.cache_get cache "b" (compute b_calls 2) <> 2 then failwith "cache should recompute evicted values";
  assert_equal_int "a computed once" 1 !a_calls;
  assert_equal_int "b computed twice" 2 !b_calls;
  assert_equal_int "c computed once" 1 !c_calls

let () =
  test_lru__test_lru ();
  test_lru__test_cache ()
