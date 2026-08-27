type entity_id = int
type attr = string
type tx = int

type entity_ref =
  | Entity_id of entity_id
  | Temp_id of string
  | CurrentTx
  | Ident of string
  | Lookup_ref of attr * value

and value =
  | Nil
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Bool of bool
  | Keyword of string
  | Uuid of string
  | Instant of int
  | Regex of string
  | Ref of entity_id
  | List of value list
  | Vector of value list
  | Map of (value * value) list
  | Set of value list
  | Tuple of value option list
  | TxRef
  | Ref_to of entity_ref


type cardinality =
  | One
  | Many

type unique =
  | Value
  | Identity

type value_type =
  | RefType
  | TupleType
  | StringType
  | KeywordType
  | NumberType
  | UuidType
  | InstantType

type schema_attr =
  { cardinality : cardinality
  ; unique : unique option
  ; indexed : bool
  ; is_component : bool
  ; no_history : bool
  ; doc : string option
  ; value_type : value_type option
  ; tuple_attrs : attr list option
  ; tuple_types : value_type list option
  }

type schema = (attr * schema_attr) list

type datom =
  { e : entity_id
  ; a : attr
  ; v : value
  ; tx : tx
  ; added : bool
  }

type index_set

type serializable_db =
  { serializable_schema : schema
  ; serializable_datoms : datom list
  ; serializable_max_eid : entity_id
  ; serializable_max_tx : tx
  }

type storage_address = string

type storage_kind = string

let storage_kind_memory = "memory"
let storage_kind_lmdb = "lmdb"
let storage_kind_sqlite = "sqlite"

type storage = Storage_handle of int

type tx_value =
  | One_value of value
  | Many_values of value list
  | One_entity of tx_entity
  | Many_entities of tx_entity list

and tx_entity =
  { db_id : entity_ref option
  ; attrs : (attr * tx_value) list
  }

and tx_op =
  | Add of entity_ref * attr * value
  | Retract of entity_ref * attr * value option
  | RetractEntity of entity_ref
  | RetractAttr of entity_ref * attr
  | Purge of entity_ref * attr * value
  | PurgeAttr of entity_ref * attr
  | PurgeEntity of entity_ref
  | CompareAndSet of entity_ref * attr * value option * value
  | Entity of tx_entity
  | Raw_datom of datom
  | InstallTxFn of entity_ref * (db -> value list -> tx_op list)
  | CallIdent of entity_ref * value list

  | Call of (db -> tx_op list)

and db =
  { db_uid : int
  ; schema : schema
  ; eavt_index : index_set
  ; aevt_index : index_set
  ; avet_index : index_set
  ; aevt_by_attr : (attr, datom array) Hashtbl.t
  ; avet_by_attr : (attr, datom array) Hashtbl.t
  ; avet_entities_by_attr_value : (attr * value, entity_id list) Hashtbl.t
  ; duplicate_datoms : datom list
  ; duplicate_aevt_datoms : datom list
  ; duplicate_avet_datoms : datom list
  ; duplicate_eavt_by_entity : (entity_id, datom list) Hashtbl.t
  ; duplicate_aevt_by_attr : (attr, datom list) Hashtbl.t
  ; duplicate_avet_by_attr : (attr, datom list) Hashtbl.t
  ; max_eid : entity_id
  ; max_datom_e : entity_id
  ; max_tx : tx
  ; store_max_tx : tx
  ; as_of_tx : tx option
  ; since_tx : tx option
  ; history : bool
  ; filter_pred : (datom -> bool) option
  ; pending_datoms : datom list
  ; storage_ref : storage option
  ; tx_fns : (entity_id * (db -> value list -> tx_op list)) list
  }

type entity =
  { id : entity_id
  ; db : db
  ; attrs : (attr * tx_value) list
  ; lookup_attr : attr -> tx_value option
  ; materialize_attrs : unit -> (attr * tx_value) list
  }

type pulled_entity =
  { pulled_id : entity_id
  ; pulled_attrs : (pull_key * pulled_value) list
  }

and pull_key = value

and pulled_value =
  | Pulled_scalar of value
  | Pulled_many of pulled_value list
  | Pulled_entity of pulled_entity

type pull_visit =
  | PullVisitAttr of entity_id * attr
  | PullVisitWildcard of entity_id
  | PullVisitReverse of attr * entity_id

type pull_selector =
  | Pull_id
  | Pull_wildcard
  | Pull_attr of attr
  | Pull_attr_default of attr * value
  | Pull_attr_limit of attr * int
  | Pull_attr_unlimited of attr
  | Pull_attr_xform of attr * (pulled_value -> pulled_value)
  | Pull_attr_default_xform of attr * value * (pulled_value -> pulled_value)
  | Pull_ref of attr * pull_selector list
  | Pull_ref_default of attr * pull_selector list * value
  | Pull_ref_limit of attr * pull_selector list * int
  | Pull_ref_unlimited of attr * pull_selector list
  | Pull_ref_xform of attr * pull_selector list * (pulled_value -> pulled_value)
  | Pull_recursive_ref of attr * pull_selector list * int option
  | Pull_reverse_ref of attr * pull_selector list
  | Pull_reverse_ref_default of attr * pull_selector list * value
  | Pull_reverse_ref_limit of attr * pull_selector list * int
  | Pull_reverse_ref_unlimited of attr * pull_selector list
  | Pull_reverse_ref_xform of attr * pull_selector list * (pulled_value -> pulled_value)
  | Pull_as of pull_selector * pull_key

type query_term =
  | QVar of string
  | QEntity of entity_id
  | QIdent of string
  | QLookupRef of attr * value
  | QAttr of attr
  | QValue of value
  | QSource of string
  | QWildcard

type query_result =
  | Result_entity of entity_id
  | Result_attr of attr
  | Result_value of value
  | Result_db of db
  | Result_pull of pulled_entity

type query_source =
  | Db_source of db
  | Relation_source of query_result list list

type value_predicate =
  | NumberValue
  | IntegerValue
  | StringValue
  | BooleanValue
  | KeywordValue

type numeric_predicate =
  | ZeroNumber
  | PositiveNumber
  | NegativeNumber
  | EvenInteger
  | OddInteger

type comparison_predicate =
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual

type equality_predicate =
  | EqualValues
  | NotEqualValues

type arithmetic_op =
  | AddNumbers
  | SubtractNumbers
  | MultiplyNumbers
  | DivideNumbers
  | IncrementNumber
  | DecrementNumber
  | QuotientNumbers
  | RemainderNumbers
  | ModuloNumbers

type extremum_op =
  | MinimumValue
  | MaximumValue

type boolean_predicate =
  | TrueValue
  | FalseValue
  | NilValue
  | SomeValue

type query_clause =
  | Pattern of query_term * query_term * query_term
  | PatternTx of query_term * query_term * query_term * query_term
  | PatternTxOp of query_term * query_term * query_term * query_term * query_term
  | SourcePattern of string * query_term * query_term * query_term
  | SourcePatternTx of string * query_term * query_term * query_term * query_term
  | SourcePatternTxOp of string * query_term * query_term * query_term * query_term * query_term
  | SourceRelationPattern of string * query_term list
  | Missing of query_term * query_term
  | SourceMissing of string * query_term * query_term
  | GetElse of query_term * query_term * query_term * string
  | SourceGetElse of string * query_term * query_term * query_term * string
  | GetSome of query_term * query_term list * string * string
  | SourceGetSome of string * query_term * query_term list * string * string
  | GetValue of query_term * query_term * string
  | GetDefaultValue of query_term * query_term * query_term * string
  | CountValue of query_term * string
  | EmptyValue of query_term
  | NotEmptyValue of query_term
  | ContainsValue of query_term * query_term
  | ValuePredicate of value_predicate * query_term
  | NumericPredicate of numeric_predicate * query_term
  | ComparisonPredicate of comparison_predicate * query_term * query_term
  | ComparisonPredicateN of comparison_predicate * query_term list
  | EqualityPredicate of equality_predicate * query_term list
  | ArithmeticValue of arithmetic_op * query_term list * string
  | CompareValue of query_term * query_term * string
  | ExtremumValue of extremum_op * query_term list * string
  | BooleanPredicate of boolean_predicate * query_term
  | BooleanNotPredicate of query_term
  | BooleanNotValue of query_term * string
  | IdentityValue of query_term * string
  | BooleanAndPredicate of query_term list
  | BooleanAndValue of query_term list * string
  | BooleanOrPredicate of query_term list
  | BooleanOrValue of query_term list * string
  | RandomValue of string
  | RandomIntValue of query_term * string
  | DifferPredicate of query_term list
  | IdenticalPredicate of query_term * query_term
  | TypeValue of query_term * string
  | MetaValue of query_term * string
  | NameValue of query_term * string
  | NamespaceValue of query_term * string
  | KeywordFromName of query_term * string
  | KeywordFromNamespaceName of query_term * query_term * string
  | StringIncludesValue of query_term * query_term
  | StringStartsWithValue of query_term * query_term
  | StringEndsWithValue of query_term * query_term
  | StringLowerCaseValue of query_term * string
  | StringUpperCaseValue of query_term * string
  | StringCapitalizeValue of query_term * string
  | StringReverseValue of query_term * string
  | StringTrimValue of query_term * string
  | StringTrimLeftValue of query_term * string
  | StringTrimRightValue of query_term * string
  | StringTrimNewlineValue of query_term * string
  | StringIndexOfValue of query_term * query_term * string
  | StringLastIndexOfValue of query_term * query_term * string
  | StringSubstringValue of query_term * query_term * query_term option * string
  | StringBuildValue of query_term list * string
  | PrintStringValue of query_term list * string
  | PrintLineStringValue of query_term list * string
  | PrStringValue of query_term list * string
  | PrnStringValue of query_term list * string
  | StringJoinPlainValue of query_term * string
  | StringJoinValue of query_term * query_term * string
  | StringReplaceValue of query_term * query_term * query_term * string
  | StringReplaceFirstValue of query_term * query_term * query_term * string
  | StringEscapeValue of query_term * query_term * string
  | RePatternValue of query_term * string
  | ReFindValue of query_term * query_term * string
  | ReMatchesValue of query_term * query_term * string
  | ReSeqValue of query_term * query_term * string
  | ReFindPredicate of query_term * query_term
  | ReMatchesPredicate of query_term * query_term
  | StringBlankValue of query_term
  | StringSplitValue of query_term * query_term * string
  | StringSplitLimitValue of query_term * query_term * query_term * string
  | StringSplitLinesValue of query_term * string
  | Ground of value * string
  | GroundCollection of value list * string
  | GroundTuple of value list * string list
  | GroundRelation of value list list * string list
  | GroundTerm of query_term * string
  | GroundTermCollection of query_term * string
  | GroundTermTuple of query_term * string list
  | GroundTermRelation of query_term * string list
  | VectorValue of query_term list * string
  | ListValue of query_term list * string
  | SetValue of query_term list * string
  | HashMapValue of query_term list * string
  | ArrayMapValue of query_term list * string
  | RangeEndValue of query_term * string
  | RangeValue of query_term * query_term * string
  | RangeStepValue of query_term * query_term * query_term * string
  | TupleFunction of query_term list * string
  | UntupleFunction of query_term * string list
  | Predicate of string * query_term list * (query_result list -> bool)
  | Function of string * query_term list * string list * (query_result list -> query_result list option)
  | DynamicPredicate of string * query_term list
  | DynamicFunction of string * query_term list * string list
  | DynamicFunctionCollection of string * query_term list * string
  | DynamicFunctionRelation of string * query_term list * string list
  | SourceClause of string * query_clause
  | Not of query_clause list
  | SourceNot of string * query_clause list
  | NotJoin of string list * query_clause list
  | SourceNotJoin of string * string list * query_clause list
  | Or of query_clause list list
  | SourceOr of string * query_clause list list
  | OrJoin of string list * query_clause list list
  | SourceOrJoin of string * string list * query_clause list list
  | OrJoinRequired of string list * string list * query_clause list list
  | SourceOrJoinRequired of string * string list * string list * query_clause list list
  | Rule of string * query_term list
  | SourceRule of string * string * query_term list

type query_rule =
  { rule_name : string
  ; rule_params : string list
  ; rule_body : query_clause list
  }

type input_binding =
  | Bind_scalar of string
  | Bind_ignore
  | Bind_collection of input_binding
  | Bind_tuple of input_binding list

type query_input =
  | Input_scalar of string * query_result
  | Input_entity_ref of string * entity_ref
  | Input_collection of string * query_result list
  | Input_collection_ignore of query_result list
  | Input_nested_collection of input_binding * query_result list
  | Input_tuple of string list * query_result list
  | Input_relation of string list * query_result list list
  | Input_nested_tuple of input_binding list * query_result list
  | Input_nested_relation of input_binding list * query_result list list
  | Input_predicate of string * (query_result list -> bool)
  | Input_function of string * (query_result list -> query_result list option)
  | Input_aggregate of string * (query_result list -> query_result)
  | Input_rules of query_rule list
  | Input_ignore
  | Input_scalar_decl of string
  | Input_collection_decl of string
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_rules_decl
  | Input_nested_collection_decl of input_binding
  | Input_tuple_decl of string list
  | Input_relation_decl of string list
  | Input_nested_tuple_decl of input_binding list
  | Input_nested_relation_decl of input_binding list
  | Input_source_decl of string

type query_arg =
  | Arg_scalar of query_result
  | Arg_entity_ref of entity_ref
  | Arg_collection of query_result list
  | Arg_tuple of query_result list
  | Arg_relation of query_result list list
  | Arg_predicate of (query_result list -> bool)
  | Arg_function of (query_result list -> query_result list option)
  | Arg_aggregate of (query_result list -> query_result)
  | Arg_rules of query_rule list

type query_form =
  | QueryFormNil
  | QueryFormBool of bool
  | QueryFormInt of int
  | QueryFormFloat of float
  | QueryFormString of string
  | QueryFormKeyword of string
  | QueryFormSymbol of string
  | QueryFormVector of query_form list
  | QueryFormList of query_form list
  | QueryFormSet of query_form list
  | QueryFormTagged of string * query_form
  | QueryFormMap of (query_form * query_form) list

type aggregate =
  | Count
  | CountDistinct
  | Distinct
  | Sum
  | Avg
  | Median
  | Variance
  | Stddev
  | Min
  | Max
  | MinN of int
  | MaxN of int
  | Rand
  | RandN of int
  | Sample of int
  | MinNVar of string
  | MaxNVar of string
  | RandNVar of string
  | SampleVar of string
  | CustomVar of string
  | Custom of (query_result list -> query_result)

type find_spec =
  | Find_var of string
  | Find_pull of string * pull_selector list
  | Find_pull_form of string * query_form
  | Find_pull_var of string * string
  | Find_pull_source of string * string * pull_selector list
  | Find_pull_source_form of string * string * query_form
  | Find_pull_source_var of string * string * string
  | Find_aggregate of aggregate * query_term list

type query =
  { find : find_spec list
  ; inputs : query_input list
  ; with_vars : string list
  ; rules : query_rule list
  ; where : query_clause list
  }

type query_return =
  | Return_relation
  | Return_collection
  | Return_tuple
  | Return_scalar

type query_return_map =
  | Return_keys of string list
  | Return_syms of string list
  | Return_strs of string list

type query_output =
  | Query_relation of query_result list list
  | Query_collection of query_result list
  | Query_tuple of query_result list option
  | Query_scalar of query_result option
  | Query_relation_maps of (value * query_result) list list
  | Query_tuple_map of (value * query_result) list option

type index =
  | Eavt
  | Aevt
  | Avet

type tx_meta = (attr * value) list

type tx_report =
  { db_before : db
  ; db_after : db
  ; tx_data : datom list
  ; tempids : (string * entity_id) list
  ; tx_meta : tx_meta
  ; purged_datoms : datom list
  }
module Compare = struct
  let split_keyword keyword =
    match String.index_opt keyword '/' with
    | None -> "", keyword
    | Some index ->
      let namespace = String.sub keyword 0 index in
      let name = String.sub keyword (index + 1) (String.length keyword - index - 1) in
      namespace, name
  
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
    | Int value -> murmur3_hash_long (Int64.of_int value)
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
    | Instant value -> murmur3_hash_long (Int64.of_int value)
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
    | Int _ | Float _ | Ref _ -> 9
    | String _ -> 10
    | Regex _ -> 11
    | Instant _ -> 12
    | Uuid _ -> 13
    | TxRef -> 14
    | Ref_to _ -> 15
  
  let rec compare_value left right =
    match left, right with
    | Int left, Int right -> compare left right
    | Float left, Float right -> compare left right
    | Int left, Float right -> compare (float_of_int left) right
    | Float left, Int right -> compare left (float_of_int right)
    | Ref left, Ref right -> compare left right
    | Int left, Ref right -> compare left right
    | Ref left, Int right -> compare left right
    | Float left, Ref right -> compare left (float_of_int right)
    | Ref left, Float right -> compare (float_of_int left) right
    | String left, String right -> compare left right
    | Symbol left, Symbol right -> compare (split_keyword left) (split_keyword right)
    | Bool left, Bool right -> compare left right
    | Uuid left, Uuid right -> compare left right
    | Instant left, Instant right -> compare left right
    | Regex left, Regex right -> compare left right
    | Nil, Nil -> 0
    | Keyword left, Keyword right -> compare (split_keyword left) (split_keyword right)
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
  
  let first_nonzero4 first second third fourth =
    if first <> 0 then first
    else if second <> 0 then second
    else if third <> 0 then third
    else fourth
  
  let compare_added left right =
    compare (if left.added then 0 else 1) (if right.added then 0 else 1)

  let compare_datom index left right =
    let tiebreak_added comparison =
      if comparison <> 0 then comparison else compare_added left right
    in
    match index with
    | Eavt ->
      tiebreak_added
        (first_nonzero4
           (compare left.e right.e)
           (compare left.a right.a)
           (compare_value left.v right.v)
           (compare left.tx right.tx))
    | Aevt ->
      tiebreak_added
        (first_nonzero4
           (compare left.a right.a)
           (compare left.e right.e)
           (compare_value left.v right.v)
           (compare left.tx right.tx))
    | Avet ->
      tiebreak_added
        (first_nonzero4
           (compare left.a right.a)
           (compare_value left.v right.v)
           (compare left.e right.e)
           (compare left.tx right.tx))
end
