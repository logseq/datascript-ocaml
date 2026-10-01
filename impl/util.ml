open Datascript_types

let rec list_equal_by equal left right =
  match left, right with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
    equal left right && list_equal_by equal left_rest right_rest
  | [], _ :: _ | _ :: _, [] -> false

let int64_to_int value =
  if Int64.compare value (Int64.of_int min_int) >= 0
     && Int64.compare value (Int64.of_int max_int) <= 0
  then Some (Int64.to_int value)
  else None

let int64_to_int_exn label value =
  match int64_to_int value with
  | Some value -> value
  | None -> invalid_arg (label ^ ": int64 out of int range: " ^ Int64.to_string value)

(* days-from-civil inverse (Howard Hinnant's civil calendar algorithm) *)
let civil_from_days days =
  let days = Int64.add days 719468L in
  let era = Int64.div (if Int64.compare days 0L >= 0 then days else Int64.sub days 146096L) 146097L in
  let day_of_era = Int64.sub days (Int64.mul era 146097L) in
  let year_of_era =
    Int64.div
      (Int64.sub day_of_era (Int64.add (Int64.div day_of_era 1460L) (Int64.sub (Int64.div day_of_era 36524L) (Int64.div day_of_era 146096L))))
      365L
  in
  let year = Int64.add year_of_era (Int64.mul era 400L) in
  let day_of_year =
    Int64.sub day_of_era
      (Int64.add (Int64.mul 365L year_of_era) (Int64.sub (Int64.div year_of_era 4L) (Int64.div year_of_era 100L)))
  in
  let month_prime = Int64.div (Int64.add (Int64.mul 5L day_of_year) 2L) 153L in
  let day = Int64.sub day_of_year (Int64.sub (Int64.div (Int64.add (Int64.mul 153L month_prime) 2L) 5L) 1L) in
  let month = if Int64.compare month_prime 10L < 0 then Int64.add month_prime 3L else Int64.sub month_prime 9L in
  let year = if Int64.compare month 2L <= 0 then Int64.add year 1L else year in
  Int64.to_int year, Int64.to_int month, Int64.to_int day

(* "YYYY-MM-DDTHH:MM:SS.mmmZ" — the canonical #inst literal form *)
let string_of_instant_millis millis =
  let days =
    let d = Int64.div millis 86400000L in
    (* floor division: shift down when millis is negative and not a whole day *)
    if Int64.compare millis 0L < 0 && Int64.rem millis 86400000L <> 0L then Int64.sub d 1L else d
  in
  let rem = Int64.sub millis (Int64.mul days 86400000L) in
  let year, month, day = civil_from_days days in
  let hour = Int64.to_int (Int64.div rem 3600000L) in
  let minute = Int64.to_int (Int64.div (Int64.rem rem 3600000L) 60000L) in
  let second = Int64.to_int (Int64.div (Int64.rem rem 60000L) 1000L) in
  let ms = Int64.to_int (Int64.rem rem 1000L) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" year month day hour minute second ms

let rec entity_ref_equal left right =
  match left, right with
  | Entity_id left, Entity_id right -> left = right
  | Temp_id left, Temp_id right -> left = right
  | CurrentTx, CurrentTx -> true
  | Ident left, Ident right -> left = right
  | Lookup_ref (left_attr, left_value), Lookup_ref (right_attr, right_value) ->
    left_attr = right_attr && value_equal left_value right_value
  | _ -> false

and value_equal left right =
  match left, right with
  | Nil, Nil -> true
  | Int64 left, Int64 right -> Int64.equal left right
  | Float left, Float right ->
    (classify_float left = FP_nan && classify_float right = FP_nan) || left = right
  | String left, String right -> left = right
  | Symbol left, Symbol right -> left = right
  | Bool left, Bool right -> left = right
  | Keyword left, Keyword right -> left = right
  | Uuid left, Uuid right -> left = right
  | Instant left, Instant right -> left = right
  | Regex left, Regex right -> left = right
  | Ref left, Ref right -> left = right
  | List left, List right -> list_equal_by value_equal left right
  | Vector left, Vector right -> list_equal_by value_equal left right
  | Set left, Set right -> list_equal_by value_equal left right
  | Map left, Map right ->
    list_equal_by
      (fun (left_key, left_value) (right_key, right_value) ->
         value_equal left_key right_key && value_equal left_value right_value)
      left
      right
  | Tuple left, Tuple right ->
    list_equal_by
      (fun left right ->
         match left, right with
         | None, None -> true
         | Some left, Some right -> value_equal left right
         | None, Some _ | Some _, None -> false)
      left
      right
  | TxRef, TxRef -> true
  | Ref_to left, Ref_to right -> entity_ref_equal left right
  | _ -> false

(* ClojureScript namespace/name split happens at the last '/', so
   (:ns a.b/c) = "a.b" and (:name a.b/c) = "c". *)
let split_keyword keyword =
  match String.rindex_opt keyword '/' with
  | None -> "", keyword
  | Some index ->
    let namespace = String.sub keyword 0 index in
    let name = String.sub keyword (index + 1) (String.length keyword - index - 1) in
    namespace, name

(* Upstream compares attr keywords with compare-keywords: no-namespace
   attrs sort first, otherwise (ns, name) pairwise.  Compare with a
   plain string sort here would order "logseq.property.class/extends"
   before "logseq.property/built-in?" ('.' < '/'), which both diverges
   from the runtime index order upstream and writes a leaf order the
   CLJS storage reader cannot binary-search.

   The (ns, name) pairwise order equals a byte compare on a normalized
   key: replace the last '/' by '\x00' (or prepend '\x00' when the attr
   has no '/').  '\x00' sorts below every byte in an attr and below '/',
   so a shorter ns that is a prefix of the other always loses to it, and
   a missing ns sorts first — exactly the pairwise order.  Keys are
   memoized by attr content since index comparators see the same small
   set of attrs over and over. *)
(* Bounded: keyword values also flow through compare_attr, so an unlimited
   vocabulary would grow this table without bound; past the cap, keys are
   recomputed per call. *)
let attr_key_tbl_cap = 65536
let attr_key_tbl : (attr, attr) Hashtbl.t = Hashtbl.create 256

let attr_key attr =
  match Hashtbl.find_opt attr_key_tbl attr with
  | Some key -> key
  | None ->
    let key =
      match String.rindex_opt attr '/' with
      | None -> "\x00" ^ attr
      | Some i ->
        String.init (String.length attr) (fun j ->
          if j = i then '\x00' else attr.[j])
    in
    if Hashtbl.length attr_key_tbl < attr_key_tbl_cap then
      Hashtbl.replace attr_key_tbl attr key;
    key

let compare_attr left right =
  if left == right || String.equal left right then 0
  else String.compare (attr_key left) (attr_key right)

let rec compare_list_items_with compare_item left right =
  match left, right with
  | [], [] -> 0
  | left :: left_rest, right :: right_rest ->
    let comparison = compare_item left right in
    if comparison <> 0 then comparison else compare_list_items_with compare_item left_rest right_rest
  | [], _ | _, [] -> 0

let compare_list_with compare_item left right =
  let length_comparison = compare (List.length left) (List.length right) in
  if length_comparison <> 0 then length_comparison
  else compare_list_items_with compare_item left right

let compare_option_with compare_item left right =
  match left, right with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right -> compare_item left right

let i32 value = Int32.of_int value
let i32_to_int value = Int32.to_int value
let i32_add left right = Int32.add left right
let i32_mul left right = Int32.mul left right
let i32_xor left right = Int32.logxor left right
let i32_shift_left value bits = Int32.shift_left value bits
let i32_shift_right value bits = Int32.shift_right value bits
let i32_shift_right_logical value bits = Int32.shift_right_logical value bits

let i32_rotate_left value bits =
  Int32.logor (Int32.shift_left value bits) (Int32.shift_right_logical value (32 - bits))

let murmur3_mix_k1 value =
  value
  |> fun value -> i32_mul value (i32 (-862048943))
  |> fun value -> i32_rotate_left value 15
  |> fun value -> i32_mul value (i32 461845907)

let murmur3_mix_h1 hash value =
  i32_xor hash value
  |> fun hash -> i32_rotate_left hash 13
  |> fun hash -> i32_add (i32_mul hash (i32 5)) (i32 (-430675100))

let murmur3_fmix hash length =
  i32_xor hash (i32 length)
  |> fun hash -> i32_xor hash (i32_shift_right_logical hash 16)
  |> fun hash -> i32_mul hash (i32 (-2048144789))
  |> fun hash -> i32_xor hash (i32_shift_right_logical hash 13)
  |> fun hash -> i32_mul hash (i32 (-1028477387))
  |> fun hash -> i32_xor hash (i32_shift_right_logical hash 16)

let murmur3_hash_int value =
  if value = 0 then 0
  else
    value
    |> i32
    |> murmur3_mix_k1
    |> murmur3_mix_h1 Int32.zero
    |> fun hash -> murmur3_fmix hash 4
    |> i32_to_int

let murmur3_hash_long value =
  if value = Int64.zero then 0
  else
    let low = Int64.to_int value |> i32 in
    let high = Int64.shift_right_logical value 32 |> Int64.to_int |> i32 in
    Int32.zero
    |> fun hash -> murmur3_mix_h1 hash (murmur3_mix_k1 low)
    |> fun hash -> murmur3_mix_h1 hash (murmur3_mix_k1 high)
    |> fun hash -> murmur3_fmix hash 8
    |> i32_to_int

let murmur3_hash_unencoded_chars text =
  let hash = ref Int32.zero in
  let index = ref 1 in
  let length = String.length text in
  while !index < length do
    let code =
      Char.code text.[!index - 1] lor (Char.code text.[!index] lsl 16)
    in
    hash := murmur3_mix_h1 !hash (murmur3_mix_k1 (i32 code));
    index := !index + 2
  done;
  if length land 1 = 1 then
    hash := i32_xor !hash (murmur3_mix_k1 (i32 (Char.code text.[length - 1])));
  murmur3_fmix !hash (2 * length) |> i32_to_int

let java_string_hash text =
  let hash = ref Int32.zero in
  String.iter
    (fun ch -> hash := i32_add (i32_mul !hash (i32 31)) (i32 (Char.code ch)))
    text;
  i32_to_int !hash

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> 10 + Char.code ch - Char.code 'a'
  | 'A' .. 'F' as ch -> 10 + Char.code ch - Char.code 'A'
  | _ -> invalid_arg "invalid UUID hex digit"

let uuid_halves uuid =
  let digits =
    uuid
    |> String.to_seq
    |> Seq.filter (( <> ) '-')
    |> List.of_seq
  in
  if List.length digits <> 32 then invalid_arg ("invalid UUID: " ^ uuid);
  let take_hex count digits =
    let rec loop acc remaining rest =
      if remaining = 0 then acc, rest
      else
        match rest with
        | [] -> invalid_arg ("invalid UUID: " ^ uuid)
        | ch :: rest ->
          loop
            (Int64.logor (Int64.shift_left acc 4) (Int64.of_int (hex_value ch)))
            (remaining - 1)
            rest
    in
    loop Int64.zero count digits
  in
  let most, rest = take_hex 16 digits in
  let least, _ = take_hex 16 rest in
  most, least

(* transit-js UUIDfromString semantics: strip '-', take the first 32
   chars as 16 hex pairs, and parseInt each pair at radix 16 (leading
   hex prefix of the pair, else 0). cljs ~u decode produces this
   canonical form for any input, so a uuid value read back from storage
   is always canonical; applying the same transform at construction
   keeps in-memory and materialized values identical. *)
let uuid_canonicalize uuid =
  let hex_opt ch =
    match ch with
    | '0' .. '9' -> Some (Char.code ch - Char.code '0')
    | 'a' .. 'f' -> Some (10 + Char.code ch - Char.code 'a')
    | 'A' .. 'F' -> Some (10 + Char.code ch - Char.code 'A')
    | _ -> None
  in
  let pair_value first second =
    match hex_opt first, second with
    | Some h1, Some c2 -> (
        match hex_opt c2 with
        | Some h2 -> (h1 * 16) + h2
        | None -> h1)
    | Some h1, None -> h1
    | None, _ -> 0
  in
  let digits =
    uuid
    |> String.to_seq
    |> Seq.filter (fun ch -> ch <> '-')
    |> List.of_seq
  in
  let bytes = Array.make 16 0 in
  let rec loop index ds =
    if index >= 16 then ()
    else
      match ds with
      | first :: second :: rest ->
          bytes.(index) <- pair_value first (Some second);
          loop (index + 1) rest
      | [ first ] -> bytes.(index) <- pair_value first None
      | [] -> ()
  in
  loop 0 digits;
  let b = Bytes.make 36 '-' in
  for i = 0 to 15 do
    let offset =
      i * 2
      + (if i >= 10 then 4
         else if i >= 8 then 3
         else if i >= 6 then 2
         else if i >= 4 then 1
         else 0)
    in
    let hi = bytes.(i) lsr 4 and lo = bytes.(i) land 0xf in
    Bytes.set b offset
      (Char.chr (hi + (if hi < 10 then Char.code '0' else Char.code 'a' - 10)));
    Bytes.set b (offset + 1)
      (Char.chr (lo + (if lo < 10 then Char.code '0' else Char.code 'a' - 10)))
  done;
  Bytes.unsafe_to_string b

let int64_low_i32 value =
  Int64.logand value 0xffffffffL |> Int64.to_int |> i32

let int64_high_i32 value =
  Int64.shift_right_logical value 32 |> int64_low_i32

let java_uuid_hash uuid =
  let most, least = uuid_halves uuid in
  i32_xor
    (i32_xor (int64_high_i32 most) (int64_low_i32 most))
    (i32_xor (int64_high_i32 least) (int64_low_i32 least))
  |> i32_to_int

let clojure_hash_combine seed hash =
  i32_xor
    (i32 seed)
    (i32_add
       (i32_add (i32 hash) (i32 (-1640531527)))
       (i32_add (i32_shift_left (i32 seed) 6) (i32_shift_right (i32 seed) 2)))
  |> i32_to_int

let clojure_symbol_hash symbol =
  let namespace, name = split_keyword symbol in
  let namespace_hash = if namespace = "" then 0 else java_string_hash namespace in
  clojure_hash_combine (murmur3_hash_unencoded_chars name) namespace_hash

let clojure_keyword_hash name =
  i32_add (i32 (clojure_symbol_hash name)) (i32 (-1640531527)) |> i32_to_int

let murmur3_mix_coll_hash hash count =
  hash
  |> i32
  |> murmur3_mix_k1
  |> murmur3_mix_h1 Int32.zero
  |> fun hash -> murmur3_fmix hash count
  |> i32_to_int

let murmur3_hash_ordered hashes =
  let count, hash =
    List.fold_left
      (fun (count, hash) value_hash ->
        count + 1, i32_add (i32_mul (i32 31) hash) (i32 value_hash))
      (0, i32 1)
      hashes
  in
  murmur3_mix_coll_hash (i32_to_int hash) count

let murmur3_hash_unordered hashes =
  let count, hash =
    List.fold_left
      (fun (count, hash) value_hash -> count + 1, i32_add hash (i32 value_hash))
      (0, Int32.zero)
      hashes
  in
  murmur3_mix_coll_hash (i32_to_int hash) count

let rec clojure_hasheq = function
  | Nil -> 0
  | Bool true -> 1231
  | Bool false -> 1237
  | Int64 value -> murmur3_hash_long value
  | Float value -> Hashtbl.hash value
  | String value -> murmur3_hash_int (java_string_hash value)
  | Symbol value -> clojure_symbol_hash value
  | Keyword value -> clojure_keyword_hash value
  | List values | Vector values -> murmur3_hash_ordered (List.map clojure_hasheq values)
  | Set values -> murmur3_hash_unordered (List.map clojure_hasheq values)
  | Map entries ->
    entries
    |> List.map (fun (key, value) -> murmur3_hash_ordered [ clojure_hasheq key; clojure_hasheq value ])
    |> murmur3_hash_unordered
  | Tuple values ->
    values
    |> List.map (function None -> 0 | Some value -> clojure_hasheq value)
    |> murmur3_hash_ordered
  | Ref value -> murmur3_hash_long (Int64.of_int value)
  | Uuid value -> java_uuid_hash value
  | Instant value -> murmur3_hash_long value
  | Regex value -> Hashtbl.hash value
  | TxRef -> Hashtbl.hash TxRef
  | Ref_to value -> Hashtbl.hash (Ref_to value)

let value_type_rank = function
  | Nil -> 0
  | Keyword _ -> 1
  | Symbol _ -> 2
  | Map _ -> 3
  | Set _ -> 4
  | List _ -> 5
  | Vector _ -> 6
  | Tuple _ -> 7
  | Bool _ -> 8
  | Int64 _ | Float _ | Ref _ -> 9
  | String _ -> 10
  | Regex _ -> 11
  | Instant _ -> 12
  | Uuid _ -> 13
  | TxRef -> 14
  | Ref_to _ -> 15

let rec compare_value left right =
  match left, right with
  | Int64 left, Int64 right -> Int64.compare left right
  | Float left, Float right -> compare left right
  | Int64 left, Float right -> compare (Int64.to_float left) right
  | Float left, Int64 right -> compare left (Int64.to_float right)
  | Ref left, Ref right -> compare left right
  | Int64 left, Ref right -> Int64.compare left (Int64.of_int right)
  | Ref left, Int64 right -> Int64.compare (Int64.of_int left) right
  | Float left, Ref right -> compare left (float_of_int right)
  | Ref left, Float right -> compare (float_of_int left) right
  | Instant left, Int64 right -> Int64.compare left right
  | Int64 left, Instant right -> Int64.compare left right
  | Instant left, Ref right -> Int64.compare left (Int64.of_int right)
  | Ref left, Instant right -> Int64.compare (Int64.of_int left) right
  | Instant left, Float right -> compare (Int64.to_float left) right
  | Float left, Instant right -> compare left (Int64.to_float right)
  | String left, String right -> compare left right
  | Symbol left, Symbol right -> compare_attr left right
  | Bool left, Bool right -> compare left right
  | Uuid left, Uuid right -> compare left right
  | Instant left, Instant right -> compare left right
  | Regex left, Regex right -> compare left right
  | Nil, Nil -> 0
  | Keyword left, Keyword right -> compare_attr left right
  | List left, List right -> compare_list_with compare_value left right
  | Vector left, Vector right -> compare_list_with compare_value left right
  | List left, Tuple right ->
    compare_list_with (compare_option_with compare_value) (List.map (fun value -> Some value) left) right
  | Set _, Set _ -> compare (clojure_hasheq left) (clojure_hasheq right)
  | Map _, Map _ -> compare (clojure_hasheq left) (clojure_hasheq right)
  | Tuple left, Tuple right -> compare_list_with (compare_option_with compare_value) left right
  | Tuple left, List right ->
    compare_list_with (compare_option_with compare_value) left (List.map (fun value -> Some value) right)
  | _ ->
    let rank_comparison = compare (value_type_rank left) (value_type_rank right) in
    if rank_comparison <> 0 then rank_comparison else compare left right

and compare_map_entry (left_key, left_value) (right_key, right_value) =
  let comparison = compare_value left_key right_key in
  if comparison <> 0 then comparison else compare_value left_value right_value

let first_nonzero comparisons =
  List.find_opt (( <> ) 0) comparisons
  |> Option.value ~default:0

(* Upstream combine-cmp short-circuits per component; keep the same
   early-exit so most comparisons only run the components needed to
   disambiguate (typically just e or a). *)
let compare_datom index left right =
  match index with
  | Eavt ->
    let cmp = compare left.e right.e in
    if cmp <> 0 then cmp
    else
      let cmp = compare_attr left.a right.a in
      if cmp <> 0 then cmp
      else
        let cmp = compare_value left.v right.v in
        if cmp <> 0 then cmp else compare left.tx right.tx
  | Aevt ->
    let cmp = compare_attr left.a right.a in
    if cmp <> 0 then cmp
    else
      let cmp = compare left.e right.e in
      if cmp <> 0 then cmp
      else
        let cmp = compare_value left.v right.v in
        if cmp <> 0 then cmp else compare left.tx right.tx
  | Avet ->
    let cmp = compare_attr left.a right.a in
    if cmp <> 0 then cmp
    else
      let cmp = compare_value left.v right.v in
      if cmp <> 0 then cmp
      else
        let cmp = compare left.e right.e in
        if cmp <> 0 then cmp else compare left.tx right.tx

let rec normalize_value = function
  | List values -> List (List.map normalize_value values)
  | Vector values -> Vector (List.map normalize_value values)
  | Map entries ->
    entries
    |> List.map (fun (key, value) -> normalize_value key, normalize_value value)
    |> List.sort_uniq compare_map_entry
    |> fun entries -> Map entries
  | Set values ->
    values
    |> List.map normalize_value
    |> List.sort_uniq compare_value
    |> fun values -> Set values
  | Tuple values ->
    Tuple (List.map (Option.map normalize_value) values)
  | value -> value

let normalize_datom_value d =
  { d with v = normalize_value d.v }
