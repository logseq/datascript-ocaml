open Datascript_types

let parse_int_slice input start length =
  int_of_string (String.sub input start length)

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = year / 400 in
  let year_of_era = year - (era * 400) in
  let month_prime = if month > 2 then month - 3 else month + 9 in
  let day_of_year = ((153 * month_prime) + 2) / 5 + day - 1 in
  let day_of_era =
    (year_of_era * 365) + (year_of_era / 4) - (year_of_era / 100) + day_of_year
  in
  (era * 146097) + day_of_era - 719468

let parse_instant_millis value =
  let length = String.length value in
  if length < 20 || value.[4] <> '-' || value.[7] <> '-' || value.[10] <> 'T'
     || value.[13] <> ':' || value.[16] <> ':'
  then invalid_arg ("invalid #inst literal: " ^ value);
  let year = parse_int_slice value 0 4 in
  let month = parse_int_slice value 5 2 in
  let day = parse_int_slice value 8 2 in
  let hour = parse_int_slice value 11 2 in
  let minute = parse_int_slice value 14 2 in
  let second = parse_int_slice value 17 2 in
  let millis, timezone_index =
    if length > 20 && value.[19] = '.' then
      let rec scan index =
        if index >= length || value.[index] = 'Z' || value.[index] = '+' || value.[index] = '-' then index
        else scan (index + 1)
      in
      let stop = scan 20 in
      let digits = String.sub value 20 (stop - 20) in
      let millis_digits =
        if String.length digits >= 3 then String.sub digits 0 3
        else digits ^ String.make (3 - String.length digits) '0'
      in
      int_of_string millis_digits, stop
    else
      0, 19
  in
  let timezone_offset_minutes =
    if timezone_index < length && value.[timezone_index] = 'Z' && timezone_index = length - 1 then
      0
    else if timezone_index + 6 = length
            && (value.[timezone_index] = '+' || value.[timezone_index] = '-')
            && value.[timezone_index + 3] = ':'
    then
      let sign = if value.[timezone_index] = '+' then 1 else -1 in
      let hours = parse_int_slice value (timezone_index + 1) 2 in
      let minutes = parse_int_slice value (timezone_index + 4) 2 in
      sign * ((hours * 60) + minutes)
    else if timezone_index + 5 = length
            && (value.[timezone_index] = '+' || value.[timezone_index] = '-')
    then
      let sign = if value.[timezone_index] = '+' then 1 else -1 in
      let hours = parse_int_slice value (timezone_index + 1) 2 in
      let minutes = parse_int_slice value (timezone_index + 3) 2 in
      sign * ((hours * 60) + minutes)
    else
      invalid_arg ("unsupported #inst timezone: " ^ value)
  in
  let days = days_from_civil year month day in
  let local_minutes = ((days * 24 + hour) * 60) + minute in
  (((local_minutes - timezone_offset_minutes) * 60 + second) * 1000) + millis

let read_edn input =
  let length = String.length input in
  let is_whitespace = function
    | ' ' | '\n' | '\r' | '\t' | ',' -> true
    | _ -> false
  in
  let is_delimiter = function
    | '[' | ']' | '(' | ')' | '{' | '}' | '"' | '\'' -> true
    | c -> is_whitespace c
  in
  let rec skip index =
    if index >= length then index
    else
      match input.[index] with
      | c when is_whitespace c -> skip (index + 1)
      | ';' ->
        let rec skip_comment index =
          if index >= length then index
          else
            match input.[index] with
            | '\n' | '\r' -> skip (index + 1)
            | _ -> skip_comment (index + 1)
        in
        skip_comment (index + 1)
      | _ -> index
  in
  let parse_token start =
    let rec scan index =
      if index >= length || is_delimiter input.[index] then index else scan (index + 1)
    in
    let stop = scan start in
    if stop = start then invalid_arg "expected EDN token";
    String.sub input start (stop - start), stop
  in
  let parse_atom token =
    match token with
    | "nil" -> QueryFormNil
    | "true" -> QueryFormBool true
    | "false" -> QueryFormBool false
    | _ when String.length token > 0 && token.[0] = ':' ->
      QueryFormKeyword (String.sub token 1 (String.length token - 1))
    | _ ->
      (match int_of_string_opt token with
       | Some value -> QueryFormInt value
       | None ->
         if String.contains token '.' || String.contains token 'e' || String.contains token 'E' then
           match float_of_string_opt token with
           | Some value -> QueryFormFloat value
           | None -> QueryFormSymbol token
         else
           QueryFormSymbol token)
  in
  let namespaced_map_namespace token =
    if String.length token >= 3 && String.sub token 0 3 = "#::" then
      let namespace = String.sub token 3 (String.length token - 3) in
      if namespace = "" then invalid_arg "auto-resolved EDN namespaced maps require a namespace";
      namespace
    else if String.length token >= 3 && String.sub token 0 2 = "#:" then
      String.sub token 2 (String.length token - 2)
    else
      invalid_arg "invalid EDN namespaced map"
  in
  let qualify_namespaced_map_name namespace name =
    match String.index_opt name '/' with
    | None -> namespace ^ "/" ^ name
    | Some index ->
      let key_namespace = String.sub name 0 index in
      if key_namespace = "_" then
        String.sub name (index + 1) (String.length name - index - 1)
      else
        name
  in
  let qualify_namespaced_map_key namespace = function
    | QueryFormKeyword name -> QueryFormKeyword (qualify_namespaced_map_name namespace name)
    | QueryFormSymbol name -> QueryFormSymbol (qualify_namespaced_map_name namespace name)
    | key -> key
  in
  let hex_digit_value = function
    | '0' .. '9' as char -> Char.code char - Char.code '0'
    | 'a' .. 'f' as char -> 10 + Char.code char - Char.code 'a'
    | 'A' .. 'F' as char -> 10 + Char.code char - Char.code 'A'
    | _ -> invalid_arg "invalid EDN unicode escape"
  in
  let parse_unicode_escape index =
    if index + 5 >= length then invalid_arg "incomplete EDN unicode escape";
    let code = ref 0 in
    for offset = 2 to 5 do
      code := (!code lsl 4) lor hex_digit_value input.[index + offset]
    done;
    if !code >= 0xD800 && !code <= 0xDFFF then invalid_arg "invalid EDN unicode escape";
    !code
  in
  let add_utf8_codepoint buffer code =
    if code <= 0x7F then
      Buffer.add_char buffer (Char.chr code)
    else if code <= 0x7FF then begin
      Buffer.add_char buffer (Char.chr (0xC0 lor (code lsr 6)));
      Buffer.add_char buffer (Char.chr (0x80 lor (code land 0x3F)))
    end else begin
      Buffer.add_char buffer (Char.chr (0xE0 lor (code lsr 12)));
      Buffer.add_char buffer (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
      Buffer.add_char buffer (Char.chr (0x80 lor (code land 0x3F)))
    end
  in
  let rec parse_form index =
    let index = skip index in
    if index >= length then invalid_arg "unexpected end of EDN input";
    match input.[index] with
    | '\'' -> parse_form (index + 1)
    | '^' ->
      let _, index = parse_form (index + 1) in
      parse_form index
    | '[' -> parse_sequence ']' (index + 1) []
    | '(' -> parse_list ')' (index + 1) []
    | '{' -> parse_map (index + 1) []
    | '"' -> parse_string (index + 1) (Buffer.create 16)
    | '#' when index + 1 < length && input.[index + 1] = '{' ->
      let form, index = parse_set (index + 2) [] in
      (match form with
       | QueryFormSet _ -> form, index
       | _ -> assert false)
    | '#' when index + 1 < length && input.[index + 1] = '"' ->
      let pattern, index = parse_string (index + 2) (Buffer.create 16) in
      QueryFormTagged ("regex", pattern), index
    | '#' when index + 1 < length && input.[index + 1] = '_' ->
      let _, index = parse_form (index + 2) in
      parse_form index
    | '#' when index + 1 < length && input.[index + 1] = ':' ->
      let token, index = parse_token index in
      let namespace = namespaced_map_namespace token in
      let form, index = parse_form index in
      (match form with
       | QueryFormMap entries ->
         QueryFormMap
           (List.map
              (fun (key, value) -> qualify_namespaced_map_key namespace key, value)
              entries),
         index
       | _ -> invalid_arg "EDN namespaced map requires a map")
    | '#' ->
      let token, index = parse_token index in
      (match token with
       | "##NaN" -> QueryFormFloat Float.nan, index
       | "##Inf" -> QueryFormFloat Float.infinity, index
       | "##-Inf" -> QueryFormFloat Float.neg_infinity, index
       | _ ->
         let tag =
           if String.length token > 1 && token.[0] = '#' then
             String.sub token 1 (String.length token - 1)
           else
             invalid_arg "expected EDN tagged literal"
         in
         let form, index = parse_form index in
         QueryFormTagged (tag, form), index)
    | _ ->
      let token, index = parse_token index in
      parse_atom token, index
  and parse_sequence closing index acc =
    let index = skip index in
    if index >= length then invalid_arg "unterminated EDN vector";
    if input.[index] = closing then QueryFormVector (List.rev acc), index + 1
    else
      let form, index = parse_form index in
      parse_sequence closing index (form :: acc)
  and parse_list closing index acc =
    let index = skip index in
    if index >= length then invalid_arg "unterminated EDN list";
    if input.[index] = closing then QueryFormList (List.rev acc), index + 1
    else
      let form, index = parse_form index in
      parse_list closing index (form :: acc)
  and parse_set index acc =
    let index = skip index in
    if index >= length then invalid_arg "unterminated EDN set";
    if input.[index] = '}' then QueryFormSet (List.rev acc), index + 1
    else
      let form, index = parse_form index in
      parse_set index (form :: acc)
  and parse_map index acc =
    let index = skip index in
    if index >= length then invalid_arg "unterminated EDN map";
    if input.[index] = '}' then QueryFormMap (List.rev acc), index + 1
    else
      let key, index = parse_form index in
      let index = skip index in
      if index >= length || input.[index] = '}' then invalid_arg "EDN map requires an even number of forms";
      let value, index = parse_form index in
      parse_map index ((key, value) :: acc)
  and parse_string index buffer =
    if index >= length then invalid_arg "unterminated EDN string";
    match input.[index] with
    | '"' -> QueryFormString (Buffer.contents buffer), index + 1
    | '\\' when index + 1 < length ->
      (match input.[index + 1] with
       | 'n' ->
         Buffer.add_char buffer '\n';
         parse_string (index + 2) buffer
       | 'r' ->
         Buffer.add_char buffer '\r';
         parse_string (index + 2) buffer
       | 't' ->
         Buffer.add_char buffer '\t';
         parse_string (index + 2) buffer
       | 'b' ->
         Buffer.add_char buffer (Char.chr 8);
         parse_string (index + 2) buffer
       | 'f' ->
         Buffer.add_char buffer (Char.chr 12);
         parse_string (index + 2) buffer
       | '"' ->
         Buffer.add_char buffer '"';
         parse_string (index + 2) buffer
       | '\\' ->
         Buffer.add_char buffer '\\';
         parse_string (index + 2) buffer
       | 'u' ->
         add_utf8_codepoint buffer (parse_unicode_escape index);
         parse_string (index + 6) buffer
       | char ->
         Buffer.add_char buffer char;
         parse_string (index + 2) buffer)
    | char ->
      Buffer.add_char buffer char;
      parse_string (index + 1) buffer
  in
  let form, index = parse_form 0 in
  if skip index <> length then invalid_arg "trailing EDN input";
  form

let rec query_value_of_form = function
  | QueryFormNil -> Nil
  | QueryFormBool value -> Bool value
  | QueryFormInt value -> Int value
  | QueryFormFloat value -> Float value
  | QueryFormString value -> String value
  | QueryFormKeyword value -> Keyword value
  | QueryFormSet values -> Set (List.map query_value_of_form values)
  | QueryFormTagged ("regex", QueryFormString value) -> Regex value
  | QueryFormTagged ("uuid", QueryFormString value) -> Uuid value
  | QueryFormTagged ("inst", QueryFormString value) -> Instant (parse_instant_millis value)
  | QueryFormTagged (tag, _) -> invalid_arg ("unsupported EDN tagged literal: " ^ tag)
  | QueryFormSymbol symbol
    when String.length symbol > 0
         && symbol.[0] <> '?'
         && symbol.[0] <> '$'
         && symbol <> "%"
         && symbol <> "_" ->
    Symbol symbol
  | QueryFormVector values -> Vector (List.map query_value_of_form values)
  | QueryFormList values -> List (List.map query_value_of_form values)
  | QueryFormMap entries -> Map (List.map (fun (key, value) -> query_value_of_form key, query_value_of_form value) entries)
  | QueryFormSymbol symbol -> invalid_arg ("cannot parse symbol as query constant: " ^ symbol)

let rec query_form_of_value = function
  | Nil -> QueryFormNil
  | Bool value -> QueryFormBool value
  | Int value -> QueryFormInt value
  | Float value -> QueryFormFloat value
  | String value -> QueryFormString value
  | Keyword value -> QueryFormKeyword value
  | Symbol value -> QueryFormSymbol value
  | List values -> QueryFormList (List.map query_form_of_value values)
  | Vector values -> QueryFormVector (List.map query_form_of_value values)
  | Set values -> QueryFormSet (List.map query_form_of_value values)
  | Tuple values ->
    QueryFormVector (List.map (function Some value -> query_form_of_value value | None -> QueryFormNil) values)
  | Map entries ->
    QueryFormMap (List.map (fun (key, value) -> query_form_of_value key, query_form_of_value value) entries)
  | Uuid value -> QueryFormTagged ("uuid", QueryFormString value)
  | Instant value -> QueryFormInt value
  | Regex value -> QueryFormTagged ("regex", QueryFormString value)
  | Ref entity_id -> QueryFormInt entity_id
  | TxRef
  | Ref_to _ ->
    invalid_arg "cannot convert value to query form"

let section_forms = function
  | QueryFormVector forms | QueryFormList forms -> forms
  | form -> [ form ]

let query_form_section key entries =
  let values =
    entries
    |> List.filter_map (fun (entry_key, value) ->
      if entry_key = QueryFormKeyword key then Some (section_forms value) else None)
  in
  match values with
  | [] -> None
  | [ forms ] -> Some (QueryFormVector forms)
  | _ -> Some (QueryFormVector (List.concat values))

let query_form_sections forms =
  let finish key values sections =
    match key with
    | None -> sections
    | Some key -> (QueryFormKeyword key, QueryFormVector (List.rev values)) :: sections
  in
  let rec collect key values sections = function
    | [] -> List.rev (finish key values sections)
    | QueryFormKeyword key' :: rest ->
      collect (Some key') [] (finish key values sections) rest
    | form :: rest ->
      (match key with
       | None -> invalid_arg "query vector must start with a keyword section"
       | Some _ -> collect key (form :: values) sections rest)
  in
  collect None [] [] forms

let query_form_map = function
  | QueryFormMap entries -> entries
  | QueryFormVector forms -> query_form_sections forms
  | _ -> invalid_arg "query should be a vector or a map"

let query_form_sequence = function
  | QueryFormVector forms | QueryFormList forms -> Some forms
  | _ -> None

let query_symbol_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then
    String.sub symbol 1 (String.length symbol - 1)
  else
    invalid_arg ("expected query variable symbol: " ^ symbol)

let cannot_parse_find_message =
  "Cannot parse :find, expected: (find-rel | find-coll | find-tuple | find-scalar)"

let find_query_symbol_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then
    String.sub symbol 1 (String.length symbol - 1)
  else
    invalid_arg cannot_parse_find_message

let query_callable_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then query_symbol_name symbol else symbol

let is_plain_input_symbol symbol =
  String.length symbol > 0
  && symbol <> "_"
  && symbol <> "%"
  && symbol.[0] <> '?'
  && symbol.[0] <> '$'

let is_query_input_symbol symbol =
  (String.length symbol > 1 && symbol.[0] = '?') || is_plain_input_symbol symbol

let query_input_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then
    String.sub symbol 1 (String.length symbol - 1)
  else if is_plain_input_symbol symbol then symbol
  else
    invalid_arg ("expected query input symbol: " ^ symbol)

let query_source_name symbol =
  if symbol = "$" then "$"
  else if String.length symbol > 1 && symbol.[0] = '$' then
    String.sub symbol 1 (String.length symbol - 1)
  else
    invalid_arg ("expected query source symbol: " ^ symbol)

let is_query_source_symbol symbol =
  String.length symbol > 0 && symbol.[0] = '$'

let is_plain_rule_symbol symbol =
  String.length symbol > 0
  && symbol <> "_"
  && symbol <> "%"
  && symbol.[0] <> '?'
  && symbol.[0] <> '$'

let aggregate_of_symbol = function
  | "count" -> Some Count
  | "count-distinct" -> Some CountDistinct
  | "distinct" -> Some Distinct
  | "sum" -> Some Sum
  | "avg" -> Some Avg
  | "median" -> Some Median
  | "variance" -> Some Variance
  | "stddev" -> Some Stddev
  | "min" -> Some Min
  | "max" -> Some Max
  | "rand" -> Some Rand
  | _ -> None

let amount_aggregate_of_symbol symbol amount =
  if amount < 0 then invalid_arg (symbol ^ " aggregate amount must be non-negative");
  match symbol with
  | "min" -> Some (MinN amount)
  | "max" -> Some (MaxN amount)
  | "rand" -> Some (RandN amount)
  | "sample" -> Some (Sample amount)
  | _ -> None

let dynamic_amount_aggregate_of_symbol symbol amount_var =
  match symbol with
  | "min" -> Some (MinNVar amount_var)
  | "max" -> Some (MaxNVar amount_var)
  | "rand" -> Some (RandNVar amount_var)
  | "sample" -> Some (SampleVar amount_var)
  | _ -> None

let parse_find_arg = function
  | QueryFormSymbol symbol when is_query_source_symbol symbol -> QSource (query_source_name symbol)
  | QueryFormSymbol symbol -> QVar (query_symbol_name symbol)
  | form -> QValue (query_value_of_form form)

let parse_find_args forms = List.map parse_find_arg forms

let parse_output_var = function
  | QueryFormSymbol "_" -> "_"
  | QueryFormSymbol symbol -> query_symbol_name symbol
  | _ -> invalid_arg "expected output variable"

let parse_output_vars = function
  | QueryFormVector forms | QueryFormList forms -> List.map parse_output_var forms
  | QueryFormSymbol symbol -> [ query_symbol_name symbol ]
  | _ -> invalid_arg "expected output variables"

let parse_flat_output_vars = function
  | QueryFormVector forms | QueryFormList forms -> Some (List.map parse_output_var forms)
  | _ -> None

let parse_collection_output_var = function
  | QueryFormVector [ QueryFormSymbol output; QueryFormSymbol "..." ]
  | QueryFormList [ QueryFormSymbol output; QueryFormSymbol "..." ] ->
    Some (query_symbol_name output)
  | _ -> None

let parse_relation_output_vars = function
  | QueryFormVector [ relation_form; QueryFormSymbol "..." ]
  | QueryFormList [ relation_form; QueryFormSymbol "..." ] ->
    (match relation_form with
     | QueryFormSymbol _ -> None
     | form -> parse_flat_output_vars form)
  | _ -> None

let nonempty_input_vars binding_name vars =
  match vars with
  | [] -> invalid_arg (binding_name ^ " :in binding requires at least one variable")
  | vars -> vars

let input_relation_vars = function
  | QueryFormVector vars | QueryFormList vars -> Some vars
  | _ -> None

let input_var_of_form = function
  | QueryFormSymbol "_" -> Some "_"
  | QueryFormSymbol symbol when String.length symbol > 1 && symbol.[0] = '?' ->
    Some (query_symbol_name symbol)
  | _ -> None

let flat_input_vars forms =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | form :: rest ->
      (match input_var_of_form form with
       | Some var -> collect (var :: acc) rest
       | None -> None)
  in
  collect [] forms

let rec parse_nested_input_binding form =
  match form with
  | QueryFormSymbol "_" -> Bind_ignore
  | QueryFormSymbol symbol -> Bind_scalar (query_symbol_name symbol)
  | _ ->
    (match query_form_sequence form with
     | Some [ binding_form; QueryFormSymbol "..." ] ->
       Bind_collection (parse_nested_input_binding binding_form)
     | Some [ (QueryFormVector _ | QueryFormList _ as relation_form) ] ->
       Bind_collection (parse_nested_input_binding relation_form)
     | Some forms ->
       (match List.map parse_nested_tuple_binding forms with
        | [] -> invalid_arg "tuple :in binding requires at least one variable"
        | bindings -> Bind_tuple bindings)
     | None -> invalid_arg "cannot parse :in binding")

and parse_nested_tuple_binding = function
  | QueryFormSymbol "_" -> Bind_ignore
  | form -> parse_nested_input_binding form

let parse_binding = parse_nested_input_binding

let nested_relation_binding = function
  | QueryFormVector forms | QueryFormList forms ->
    (match flat_input_vars forms with
     | Some _ -> None
     | None ->
       (match parse_nested_input_binding (QueryFormVector forms) with
        | Bind_tuple bindings -> Some bindings
        | _ -> None))
  | _ -> None

let parse_input_binding = function
  | QueryFormSymbol "%" -> Some Input_rules_decl
  | QueryFormSymbol "_" -> Some Input_ignore_decl
  | QueryFormSymbol symbol when is_query_source_symbol symbol ->
    Some (Input_source_decl (query_source_name symbol))
  | QueryFormSymbol symbol -> Some (Input_scalar_decl (query_input_name symbol))
  | form ->
    (match query_form_sequence form with
     | Some [ QueryFormSymbol "_"; QueryFormSymbol "..." ] ->
       Some Input_collection_ignore_decl
     | Some [ QueryFormSymbol symbol; QueryFormSymbol "..." ] ->
       Some (Input_collection_decl (query_symbol_name symbol))
     | Some [ relation_form; QueryFormSymbol "..." ] ->
       (match input_relation_vars relation_form with
        | Some vars ->
          (match flat_input_vars vars with
           | Some vars -> Some (Input_relation_decl (vars |> nonempty_input_vars "relation"))
           | None ->
             (match nested_relation_binding relation_form with
              | Some bindings -> Some (Input_nested_relation_decl bindings)
              | None -> Some (Input_nested_collection_decl (parse_nested_input_binding relation_form))))
        | None -> Some (Input_nested_collection_decl (parse_nested_input_binding relation_form)))
     | Some [ relation_form ] ->
       (match input_relation_vars relation_form with
        | Some vars ->
          (match flat_input_vars vars with
           | Some vars -> Some (Input_relation_decl (vars |> nonempty_input_vars "relation"))
           | None ->
             (match nested_relation_binding relation_form with
              | Some bindings -> Some (Input_nested_relation_decl bindings)
              | None -> invalid_arg "cannot parse :in binding"))
        | None -> Some (Input_tuple_decl ([ relation_form ] |> List.map parse_output_var |> nonempty_input_vars "tuple")))
     | Some vars ->
       (match flat_input_vars vars with
        | Some vars -> Some (Input_tuple_decl (vars |> nonempty_input_vars "tuple"))
        | None ->
          (match parse_nested_input_binding form with
           | Bind_tuple bindings -> Some (Input_nested_tuple_decl bindings)
           | _ -> invalid_arg "cannot parse :in binding"))
     | None -> invalid_arg "cannot parse :in binding")

let parse_inputs = function
  | Some (QueryFormVector inputs | QueryFormList inputs) -> List.filter_map parse_input_binding inputs
  | Some _ -> invalid_arg "query :in must be a vector or list"
  | None -> []

let parse_in form = parse_inputs (Some form)

let input_declares_rules_var = function
  | Some (QueryFormVector inputs) | Some (QueryFormList inputs) ->
    List.exists (function QueryFormSymbol "%" -> true | _ -> false) inputs
  | Some _ | None -> false

let ensure_distinct_input_rules_var = function
  | Some (QueryFormVector inputs) | Some (QueryFormList inputs) ->
    let count =
      List.fold_left
        (fun count -> function
          | QueryFormSymbol "%" -> count + 1
          | _ -> count)
        0
        inputs
    in
    if count > 1 then invalid_arg "Vars used in :in should be distinct"
  | Some _ | None -> ()

let parse_with_var = function
  | QueryFormSymbol "_" -> invalid_arg "Cannot parse :with clause"
  | QueryFormSymbol symbol -> query_symbol_name symbol
  | _ -> invalid_arg "Cannot parse :with clause"

let parse_with_section = function
  | Some (QueryFormVector vars | QueryFormList vars) -> List.map parse_with_var vars
  | Some _ -> invalid_arg "query :with must be a vector or list"
  | None -> []

let parse_with form = parse_with_section (Some form)

let parse_return_map_labels section_name = function
  | QueryFormVector labels | QueryFormList labels ->
    (match labels with
     | [] -> invalid_arg (":" ^ section_name ^ " requires at least one label")
     | labels ->
       List.map
         (function
           | QueryFormSymbol label -> label
           | _ -> invalid_arg (":" ^ section_name ^ " labels must be symbols"))
         labels)
  | _ -> invalid_arg (":" ^ section_name ^ " must be a vector or list")

let parse_return_map_section entries =
  let sections =
    [ "keys", (fun labels -> Return_keys labels)
    ; "syms", (fun labels -> Return_syms labels)
    ; "strs", (fun labels -> Return_strs labels)
    ]
    |> List.filter_map (fun (section_name, make_return_map) ->
      query_form_section section_name entries
      |> Option.map (fun section -> make_return_map (parse_return_map_labels section_name section)))
  in
  match sections with
  | [] -> None
  | [ section ] -> Some section
  | _ -> invalid_arg "Only one of :keys/:syms/:strs must be present"

let lookup_ref_of_form = function
  | QueryFormVector [ QueryFormKeyword attr; value ] ->
    Some (attr, query_value_of_form value)
  | _ -> None

let parse_pattern_term
      ?(entity_position = false)
      ?(attr_position = false)
      ?(lookup_ref_position = false)
      ?(source_position = true)
      form =
  match lookup_ref_of_form form with
  | Some (attr, value) when entity_position -> QLookupRef (attr, value)
  | Some (attr, value) when lookup_ref_position -> QValue (Ref_to (Lookup_ref (attr, value)))
  | _ ->
    (match form with
     | QueryFormSymbol "_" -> QWildcard
     | QueryFormSymbol symbol when String.length symbol > 0 && symbol.[0] = '?' ->
       QVar (query_symbol_name symbol)
     | QueryFormSymbol symbol when source_position && is_query_source_symbol symbol ->
       QSource (query_source_name symbol)
     | QueryFormSymbol symbol -> QValue (Symbol symbol)
     | QueryFormInt entity_id when entity_position -> QEntity entity_id
     | QueryFormKeyword attr when attr_position -> QAttr attr
     | QueryFormKeyword value -> QValue (Keyword value)
     | QueryFormInt value -> QValue (Int value)
     | QueryFormFloat value -> QValue (Float value)
     | QueryFormString value -> QValue (String value)
     | QueryFormBool value -> QValue (Bool value)
     | QueryFormNil -> QValue Nil
     | QueryFormVector _ | QueryFormList _ | QueryFormSet _ | QueryFormTagged _ | QueryFormMap _ as form ->
       QValue (query_value_of_form form))

let comparison_predicate_of_symbol = function
  | "<" -> Some LessThan
  | ">" -> Some GreaterThan
  | "<=" -> Some LessOrEqual
  | ">=" -> Some GreaterOrEqual
  | _ -> None

let value_predicate_of_symbol = function
  | "number?" -> Some NumberValue
  | "integer?" -> Some IntegerValue
  | "string?" -> Some StringValue
  | "boolean?" -> Some BooleanValue
  | "keyword?" -> Some KeywordValue
  | _ -> None

let numeric_predicate_of_symbol = function
  | "zero?" -> Some ZeroNumber
  | "pos?" -> Some PositiveNumber
  | "neg?" -> Some NegativeNumber
  | "even?" -> Some EvenInteger
  | "odd?" -> Some OddInteger
  | _ -> None

let boolean_predicate_of_symbol = function
  | "true?" -> Some TrueValue
  | "false?" -> Some FalseValue
  | "nil?" -> Some NilValue
  | "some?" -> Some SomeValue
  | _ -> None

let unary_string_predicate_clause_of_symbol = function
  | "clojure.string/blank?" -> Some (fun term -> StringBlankValue term)
  | _ -> None

let binary_string_predicate_clause_of_symbol = function
  | "clojure.string/includes?" -> Some (fun left right -> StringIncludesValue (left, right))
  | "clojure.string/starts-with?" -> Some (fun left right -> StringStartsWithValue (left, right))
  | "clojure.string/ends-with?" -> Some (fun left right -> StringEndsWithValue (left, right))
  | _ -> None

let equality_predicate_of_symbol = function
  | "=" | "==" -> Some EqualValues
  | "!=" | "not=" -> Some NotEqualValues
  | _ -> None

let arithmetic_op_of_symbol = function
  | "+" -> Some AddNumbers
  | "-" -> Some SubtractNumbers
  | "*" -> Some MultiplyNumbers
  | "/" -> Some DivideNumbers
  | "quot" -> Some QuotientNumbers
  | "rem" -> Some RemainderNumbers
  | "mod" -> Some ModuloNumbers
  | "inc" -> Some IncrementNumber
  | "dec" -> Some DecrementNumber
  | _ -> None

let query_attr_name = function
  | QueryFormKeyword attr | QueryFormString attr -> attr
  | _ -> invalid_arg "expected query attribute"

let parse_attr_term = function
  | QueryFormKeyword attr | QueryFormString attr -> QAttr attr
  | form -> parse_pattern_term ~source_position:false form

let parse_data_pattern_clause = function
  | [ e; (QueryFormKeyword _ | QueryFormString _ as a) ] ->
    Pattern
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , QWildcard )
  | [ e; a; v ] ->
    Pattern
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v )
  | [ e; a; v; tx ] ->
    PatternTx
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx )
  | [ e; a; v; tx; op ] ->
    PatternTxOp
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx
      , parse_pattern_term ~source_position:false op )
  | [] -> invalid_arg "pattern could not be empty"
  | terms -> SourceRelationPattern ("$", List.map (parse_pattern_term ~source_position:false) terms)

let parse_rule_expr rule_name args =
  match args with
  | [] -> invalid_arg "rule-expr requires at least one argument"
  | args -> rule_name, List.map (parse_pattern_term ~source_position:false) args

let parse_source_pattern_clause source_name terms =
  match terms with
  | [ (QueryFormList (QueryFormSymbol rule_name :: args)
      | QueryFormVector (QueryFormSymbol rule_name :: args))
    ] ->
    let rule_name, args = parse_rule_expr rule_name args in
    SourceRule (source_name, rule_name, args)
  | QueryFormSymbol rule_name :: args when is_plain_rule_symbol rule_name ->
    let rule_name, args = parse_rule_expr rule_name args in
    SourceRule (source_name, rule_name, args)
  | [ e; a; v ] ->
    SourcePattern
      ( source_name
      , parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v )
  | [ e; a; v; tx ] ->
    SourcePatternTx
      ( source_name
      , parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx )
  | [ e; a; v; tx; op ] ->
    SourcePatternTxOp
      ( source_name
      , parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx
      , parse_pattern_term ~source_position:false op )
  | [] -> invalid_arg "source pattern could not be empty"
  | terms -> SourceRelationPattern (source_name, List.map (parse_pattern_term ~source_position:false) terms)

let parse_missing_clause = function
  | [ entity; attr ] ->
    Missing (parse_pattern_term ~entity_position:true entity, parse_attr_term attr)
  | QueryFormSymbol source :: entity :: attr :: [] when is_query_source_symbol source ->
    SourceMissing
      (query_source_name source, parse_pattern_term ~entity_position:true entity, parse_attr_term attr)
  | _ -> invalid_arg "missing? requires an entity and an attribute"

let parse_get_else_clause args output =
  let output_var = query_symbol_name output in
  match args with
  | [ entity; attr; default ] ->
    GetElse
      ( parse_pattern_term ~entity_position:true entity
      , parse_attr_term attr
      , parse_pattern_term ~source_position:false default
      , output_var )
  | QueryFormSymbol source :: entity :: attr :: default :: [] when is_query_source_symbol source ->
    SourceGetElse
      ( query_source_name source
      , parse_pattern_term ~entity_position:true entity
      , parse_attr_term attr
      , parse_pattern_term ~source_position:false default
      , output_var )
  | _ -> invalid_arg "get-else requires an entity, an attribute, a default, and an output"

let parse_two_output_vars = function
  | QueryFormVector [ QueryFormSymbol left; QueryFormSymbol right ]
  | QueryFormList [ QueryFormSymbol left; QueryFormSymbol right ] ->
    query_symbol_name left, query_symbol_name right
  | _ -> invalid_arg "expected two output variables"

let parse_get_some_clause args output =
  let attr_var, value_var = parse_two_output_vars output in
  let build entity attrs =
    match attrs with
    | [] -> invalid_arg "get-some requires at least one attribute"
    | attrs ->
      GetSome
        ( parse_pattern_term ~entity_position:true entity
        , List.map parse_attr_term attrs
        , attr_var
        , value_var )
  in
  match args with
  | QueryFormSymbol source :: entity :: attrs when is_query_source_symbol source ->
    (match attrs with
     | [] -> invalid_arg "get-some requires at least one attribute"
     | attrs ->
       SourceGetSome
         ( query_source_name source
         , parse_pattern_term ~entity_position:true entity
         , List.map parse_attr_term attrs
         , attr_var
         , value_var ))
  | entity :: attrs -> build entity attrs
  | [] -> invalid_arg "get-some requires an entity and attributes"

let parse_get_clause args output =
  match args with
  | [ map; key ] -> GetValue (parse_pattern_term map, parse_pattern_term key, query_symbol_name output)
  | [ map; key; default ] ->
    GetDefaultValue
      (parse_pattern_term map, parse_pattern_term key, parse_pattern_term default, query_symbol_name output)
  | _ -> invalid_arg "get requires a map, a key, an optional default, and an output"

let parse_core_value_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "identity", [ term ] -> IdentityValue (parse_pattern_term term, output_var)
  | "and", terms -> BooleanAndValue (List.map parse_pattern_term terms, output_var)
  | "or", terms -> BooleanOrValue (List.map parse_pattern_term terms, output_var)
  | "compare", [ left; right ] -> CompareValue (parse_pattern_term left, parse_pattern_term right, output_var)
  | "compare", _ -> invalid_arg "compare requires two arguments"
  | "min", [] | "max", [] -> invalid_arg (symbol ^ " requires at least one argument")
  | "min", terms -> ExtremumValue (MinimumValue, List.map parse_pattern_term terms, output_var)
  | "max", terms -> ExtremumValue (MaximumValue, List.map parse_pattern_term terms, output_var)
  | "rand", [] -> RandomValue output_var
  | "rand", _ -> invalid_arg "rand requires no arguments"
  | "rand-int", [ bound ] -> RandomIntValue (parse_pattern_term bound, output_var)
  | "rand-int", _ -> invalid_arg "rand-int requires one argument"
  | _ -> DynamicFunction (query_callable_name symbol, List.map parse_pattern_term args, [ output_var ])

let parse_collection_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "vector", terms -> VectorValue (List.map parse_pattern_term terms, output_var)
  | "list", terms -> ListValue (List.map parse_pattern_term terms, output_var)
  | "set", terms -> SetValue (List.map parse_pattern_term terms, output_var)
  | "hash-map", terms ->
    if List.length terms mod 2 <> 0 then invalid_arg "hash-map requires an even number of arguments";
    HashMapValue (List.map parse_pattern_term terms, output_var)
  | "array-map", terms ->
    if List.length terms mod 2 <> 0 then invalid_arg "array-map requires an even number of arguments";
    ArrayMapValue (List.map parse_pattern_term terms, output_var)
  | "range", [ end_ ] -> RangeEndValue (parse_pattern_term end_, output_var)
  | "range", [ start; end_ ] -> RangeValue (parse_pattern_term start, parse_pattern_term end_, output_var)
  | "range", [ start; end_; step ] ->
    RangeStepValue (parse_pattern_term start, parse_pattern_term end_, parse_pattern_term step, output_var)
  | "range", _ -> invalid_arg "range requires one, two, or three arguments"
  | "tuple", terms -> TupleFunction (List.map parse_pattern_term terms, output_var)
  | _ -> parse_core_value_function symbol args output

let parse_flat_value_function symbol args output_vars =
  match symbol, args with
  | "identity", [ term ] -> GroundTermTuple (parse_pattern_term term, output_vars)
  | _ -> DynamicFunction (query_callable_name symbol, List.map parse_pattern_term args, output_vars)

let parse_collection_value_function symbol args output_var =
  match symbol, args with
  | "identity", [ term ] -> GroundTermCollection (parse_pattern_term term, output_var)
  | _ -> DynamicFunctionCollection (query_callable_name symbol, List.map parse_pattern_term args, output_var)

let parse_relation_value_function symbol args output_vars =
  match symbol, args with
  | "identity", [ term ] -> GroundTermRelation (parse_pattern_term term, output_vars)
  | _ -> DynamicFunctionRelation (query_callable_name symbol, List.map parse_pattern_term args, output_vars)

let ground_values_of_form = function
  | QueryFormVector values | QueryFormList values -> List.map query_value_of_form values
  | _ -> invalid_arg "ground tuple output requires a vector or list value"

let ground_relation_rows_of_form = function
  | QueryFormVector rows | QueryFormList rows -> List.map ground_values_of_form rows
  | _ -> invalid_arg "ground relation output requires a vector or list value"

let dynamic_ground_term = function
  | QueryFormSymbol symbol
    when (String.length symbol > 0 && symbol.[0] = '?') || is_query_source_symbol symbol ->
    Some (parse_pattern_term (QueryFormSymbol symbol))
  | _ -> None

let parse_ground_function args output =
  match args, output with
  | [ value_form ], QueryFormSymbol output_symbol when Option.is_some (dynamic_ground_term value_form) ->
    GroundTerm (Option.get (dynamic_ground_term value_form), parse_output_var (QueryFormSymbol output_symbol))
  | [ value_form ], QueryFormVector [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ]
    when Option.is_some (dynamic_ground_term value_form) ->
    GroundTermCollection (Option.get (dynamic_ground_term value_form), query_symbol_name output_symbol)
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ]
    when Option.is_some (dynamic_ground_term value_form) ->
    GroundTermRelation (Option.get (dynamic_ground_term value_form), parse_output_vars output_form)
  | [ value_form ], (QueryFormVector _ | QueryFormList _ as output_form)
    when Option.is_some (dynamic_ground_term value_form) ->
    GroundTermTuple (Option.get (dynamic_ground_term value_form), parse_output_vars output_form)
  | [ value_form ], QueryFormSymbol output_symbol ->
    Ground (query_value_of_form value_form, parse_output_var (QueryFormSymbol output_symbol))
  | [ value_form ], QueryFormVector [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ] ->
    GroundCollection (ground_values_of_form value_form, query_symbol_name output_symbol)
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ] ->
    GroundRelation (ground_relation_rows_of_form value_form, parse_output_vars output_form)
  | [ value_form ], (QueryFormVector _ | QueryFormList _ as output_form) ->
    GroundTuple (ground_values_of_form value_form, parse_output_vars output_form)
  | [ _ ], _ -> invalid_arg "ground output must be a variable or tuple binding"
  | _ -> invalid_arg "ground requires one argument"

let parse_value_metadata_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "type", [ term ] -> TypeValue (parse_pattern_term term, output_var)
  | "meta", [ term ] -> MetaValue (parse_pattern_term term, output_var)
  | "name", [ term ] -> NameValue (parse_pattern_term term, output_var)
  | "namespace", [ term ] -> NamespaceValue (parse_pattern_term term, output_var)
  | "keyword", [ term ] -> KeywordFromName (parse_pattern_term term, output_var)
  | "keyword", [ namespace; name ] ->
    KeywordFromNamespaceName (parse_pattern_term namespace, parse_pattern_term name, output_var)
  | ("type" | "meta" | "name" | "namespace"), _ -> invalid_arg (symbol ^ " requires one argument")
  | "keyword", _ -> invalid_arg "keyword requires one or two arguments"
  | _ -> parse_collection_function symbol args output

let parse_string_transform_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "clojure.string/lower-case", [ term ] -> StringLowerCaseValue (parse_pattern_term term, output_var)
  | "clojure.string/upper-case", [ term ] -> StringUpperCaseValue (parse_pattern_term term, output_var)
  | "clojure.string/capitalize", [ term ] -> StringCapitalizeValue (parse_pattern_term term, output_var)
  | "clojure.string/reverse", [ term ] -> StringReverseValue (parse_pattern_term term, output_var)
  | "clojure.string/trim", [ term ] -> StringTrimValue (parse_pattern_term term, output_var)
  | "clojure.string/triml", [ term ] -> StringTrimLeftValue (parse_pattern_term term, output_var)
  | "clojure.string/trimr", [ term ] -> StringTrimRightValue (parse_pattern_term term, output_var)
  | "clojure.string/trim-newline", [ term ] -> StringTrimNewlineValue (parse_pattern_term term, output_var)
  | "clojure.string/index-of", [ value; needle ] ->
    StringIndexOfValue (parse_pattern_term value, parse_pattern_term needle, output_var)
  | "clojure.string/last-index-of", [ value; needle ] ->
    StringLastIndexOfValue (parse_pattern_term value, parse_pattern_term needle, output_var)
  | "str", terms -> StringBuildValue (List.map parse_pattern_term terms, output_var)
  | "print-str", terms -> PrintStringValue (List.map parse_pattern_term terms, output_var)
  | "println-str", terms -> PrintLineStringValue (List.map parse_pattern_term terms, output_var)
  | "pr-str", terms -> PrStringValue (List.map parse_pattern_term terms, output_var)
  | "prn-str", terms -> PrnStringValue (List.map parse_pattern_term terms, output_var)
  | "clojure.string/join", [ collection ] ->
    StringJoinPlainValue (parse_pattern_term collection, output_var)
  | "clojure.string/join", [ separator; collection ] ->
    StringJoinValue (parse_pattern_term separator, parse_pattern_term collection, output_var)
  | "clojure.string/replace", [ value; pattern; replacement ] ->
    StringReplaceValue
      (parse_pattern_term value, parse_pattern_term pattern, parse_pattern_term replacement, output_var)
  | "clojure.string/replace-first", [ value; pattern; replacement ] ->
    StringReplaceFirstValue
      (parse_pattern_term value, parse_pattern_term pattern, parse_pattern_term replacement, output_var)
  | "clojure.string/escape", [ value; replacements ] ->
    StringEscapeValue (parse_pattern_term value, parse_pattern_term replacements, output_var)
  | "re-pattern", [ pattern ] -> RePatternValue (parse_pattern_term pattern, output_var)
  | "re-find", [ pattern; value ] ->
    ReFindValue (parse_pattern_term pattern, parse_pattern_term value, output_var)
  | "re-matches", [ pattern; value ] ->
    ReMatchesValue (parse_pattern_term pattern, parse_pattern_term value, output_var)
  | "re-seq", [ pattern; value ] ->
    ReSeqValue (parse_pattern_term pattern, parse_pattern_term value, output_var)
  | "clojure.string/split", [ value; separator ] ->
    StringSplitValue (parse_pattern_term value, parse_pattern_term separator, output_var)
  | "clojure.string/split", [ value; separator; limit ] ->
    StringSplitLimitValue
      (parse_pattern_term value, parse_pattern_term separator, parse_pattern_term limit, output_var)
  | "clojure.string/split-lines", [ value ] ->
    StringSplitLinesValue (parse_pattern_term value, output_var)
  | "subs", [ value; start ] ->
    StringSubstringValue (parse_pattern_term value, parse_pattern_term start, None, output_var)
  | "subs", [ value; start; end_ ] ->
    StringSubstringValue
      (parse_pattern_term value, parse_pattern_term start, Some (parse_pattern_term end_), output_var)
  | ( "clojure.string/lower-case"
    | "clojure.string/upper-case"
    | "clojure.string/capitalize"
    | "clojure.string/reverse"
    | "clojure.string/trim"
    | "clojure.string/triml"
    | "clojure.string/trimr"
    | "clojure.string/trim-newline" ), _ ->
    invalid_arg (symbol ^ " requires one argument")
  | ("clojure.string/index-of" | "clojure.string/last-index-of"), _ ->
    invalid_arg (symbol ^ " requires two arguments")
  | "clojure.string/join", _ -> invalid_arg "clojure.string/join requires one or two arguments"
  | ("clojure.string/replace" | "clojure.string/replace-first"), _ ->
    invalid_arg (symbol ^ " requires three arguments")
  | "clojure.string/escape", _ -> invalid_arg "clojure.string/escape requires two arguments"
  | "re-pattern", _ -> invalid_arg "re-pattern requires one argument"
  | ("re-find" | "re-matches" | "re-seq"), _ ->
    invalid_arg (symbol ^ " requires two arguments")
  | "clojure.string/split", _ -> invalid_arg "clojure.string/split requires two or three arguments"
  | "clojure.string/split-lines", _ -> invalid_arg "clojure.string/split-lines requires one argument"
  | "subs", _ -> invalid_arg "subs requires two or three arguments"
  | _ -> parse_value_metadata_function symbol args output


type query_context =
  { empty_db : unit -> db
  ; parse_pull_pattern : db -> query_form -> pull_selector list
  ; value_of_query_result : query_result -> value option
  ; string_is_blank : string -> bool
  ; string_includes : string -> string -> bool
  ; string_starts_with : string -> string -> bool
  ; string_ends_with : string -> string -> bool
  ; matches_value_predicate : value_predicate -> value -> bool
  ; matches_numeric_predicate : numeric_predicate -> value -> bool
  ; matches_boolean_predicate : boolean_predicate -> query_result -> bool
  ; comparison_chain_matches : comparison_predicate -> value list -> bool
  ; all_values_equal : value list -> bool
  ; value_has_count : int -> value -> bool
  ; value_is_not_empty : value -> bool
  ; value_contains : value -> value -> bool
  ; split_at : int -> value list -> value list * value list
  ; values_equal : value -> value -> bool
  }

let vars_of_clause = Query.vars_of_clause
let vars_of_branch = Query.vars_of_branch

let parse_find_form context ?(defer_pull_patterns = false) ?default_pull_db ?pull_db_for_source form =
  let default_pull_db = Option.value default_pull_db ~default:(context.empty_db ()) in
  let pull_db_for_source = Option.value pull_db_for_source ~default:(fun _ -> context.empty_db ()) in
  match form with
  | QueryFormSymbol symbol -> Find_var (find_query_symbol_name symbol)
  | form ->
    (match query_form_sequence form with
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol var; QueryFormSymbol pattern_var ]
       when is_query_input_symbol pattern_var && pattern_var <> "*" ->
       Find_pull_var (query_symbol_name var, query_input_name pattern_var)
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol var; pattern ] ->
       let var = query_symbol_name var in
       if defer_pull_patterns
       then Find_pull_form (var, pattern)
       else Find_pull (var, context.parse_pull_pattern default_pull_db pattern)
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol source; QueryFormSymbol var; QueryFormSymbol pattern_var ]
       when is_query_source_symbol source && is_query_input_symbol pattern_var && pattern_var <> "*" ->
       Find_pull_source_var (query_source_name source, query_symbol_name var, query_input_name pattern_var)
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol source; QueryFormSymbol var; pattern ]
       when is_query_source_symbol source ->
       let source_name = query_source_name source in
       let var = query_symbol_name var in
       if defer_pull_patterns
       then Find_pull_source_form (source_name, var, pattern)
       else Find_pull_source (source_name, var, context.parse_pull_pattern (pull_db_for_source source_name) pattern)
     | Some [ QueryFormSymbol aggregate; QueryFormSymbol var ] ->
       (match aggregate_of_symbol aggregate with
        | Some aggregate -> Find_aggregate (aggregate, [ QVar (query_symbol_name var) ])
        | None -> invalid_arg "find elements must be variable symbols")
     | Some (QueryFormSymbol "aggregate" :: QueryFormSymbol aggregate_var :: args)
       when String.length aggregate_var > 0 && aggregate_var.[0] = '?' ->
       (match args with
        | [] -> invalid_arg "aggregate custom aggregate requires at least one argument"
        | args -> Find_aggregate (CustomVar (query_symbol_name aggregate_var), parse_find_args args))
     | Some [ QueryFormSymbol aggregate; QueryFormInt amount; QueryFormSymbol var ] ->
       (match amount_aggregate_of_symbol aggregate amount with
        | Some aggregate -> Find_aggregate (aggregate, [ QVar (query_symbol_name var) ])
        | None -> invalid_arg "find elements must be variable symbols")
     | Some [ QueryFormSymbol aggregate; QueryFormSymbol amount_var; QueryFormSymbol var ]
       when String.length amount_var > 0 && amount_var.[0] = '?' ->
       (match dynamic_amount_aggregate_of_symbol aggregate (query_symbol_name amount_var) with
        | Some aggregate -> Find_aggregate (aggregate, [ QVar (query_symbol_name var) ])
        | None -> invalid_arg "find elements must be variable symbols")
     | Some [ QueryFormSymbol aggregate; _; QueryFormSymbol _ ] ->
       (match amount_aggregate_of_symbol aggregate 0 with
        | Some _ -> invalid_arg (aggregate ^ " aggregate amount must be an integer literal")
        | None -> invalid_arg "find elements must be variable symbols")
     | Some (QueryFormSymbol aggregate_name :: args) ->
       (match aggregate_of_symbol aggregate_name with
        | Some aggregate ->
          (match args with
           | [] -> invalid_arg (aggregate_name ^ " aggregate requires at least one argument")
           | args -> Find_aggregate (aggregate, parse_find_args args))
        | None -> invalid_arg "find elements must be variable symbols")
     | Some _ | None -> invalid_arg "find elements must be variable symbols")

let parse_find_relation context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source = function
  | Some (QueryFormVector forms | QueryFormList forms) ->
    List.map (parse_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source) forms
  | Some _ -> invalid_arg "query :find must be a vector"
  | None -> invalid_arg "query requires :find"

let is_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form =
  match parse_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form with
  | _ -> true
  | exception Invalid_argument _ -> false

let parse_find_return context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source = function
  | Some (QueryFormVector [ (QueryFormVector [ form; QueryFormSymbol "..." ]
                           | QueryFormList [ form; QueryFormSymbol "..." ]) ])
  | Some (QueryFormList [ (QueryFormVector [ form; QueryFormSymbol "..." ]
                         | QueryFormList [ form; QueryFormSymbol "..." ]) ]) ->
    Return_collection, [ parse_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form ]
  | Some (QueryFormVector [ form; QueryFormSymbol "." ])
  | Some (QueryFormList [ form; QueryFormSymbol "." ]) ->
    Return_scalar, [ parse_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form ]
  | Some (QueryFormVector [ ((QueryFormVector _ | QueryFormList _) as form) ])
  | Some (QueryFormList [ ((QueryFormVector _ | QueryFormList _) as form) ])
    when not (is_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form) ->
    (match form with
     | QueryFormVector forms
     | QueryFormList forms ->
       Return_tuple, List.map (parse_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source) forms
     | _ -> assert false)
  | Some form when is_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form ->
    Return_relation, [ parse_find_form context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form ]
  | form -> Return_relation, parse_find_relation context ?defer_pull_patterns ?default_pull_db ?pull_db_for_source form

let parse_find context form = parse_find_return context (Some form)



let unary_string_predicate_of_symbol context = function
  | "clojure.string/blank?" -> Some context.string_is_blank
  | _ -> None


let binary_string_predicate_of_symbol context = function
  | "clojure.string/includes?" -> Some context.string_includes
  | "clojure.string/starts-with?" -> Some context.string_starts_with
  | "clojure.string/ends-with?" -> Some context.string_ends_with
  | _ -> None


let query_results_as_values context results =
  let ( let* ) = Option.bind in
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | result :: rest ->
      let* value = context.value_of_query_result result in
      collect (value :: acc) rest
  in
  collect [] results

let one_arg_message symbol = "complement " ^ symbol ^ " requires one argument"

let two_arg_message symbol = "complement " ^ symbol ^ " requires two arguments"

let parse_complement_predicate_clause context symbol args =
  let terms = List.map parse_pattern_term args in
  let clause predicate = Predicate ("complement " ^ symbol, terms, fun results -> not (predicate results)) in
  let unary_result_predicate predicate =
    match args with
    | [ _ ] -> clause (function [ result ] -> predicate result | _ -> false)
    | _ -> invalid_arg (one_arg_message symbol)
  in
  let unary_value_predicate predicate =
    unary_result_predicate (function Result_value value -> predicate value | _ -> false)
  in
  let binary_string_predicate predicate =
    match args with
    | [ _; _ ] ->
      clause (function
        | [ Result_value (String left); Result_value (String right) ] -> predicate left right
        | _ -> false)
    | _ -> invalid_arg (two_arg_message symbol)
  in
  match
    value_predicate_of_symbol symbol,
    numeric_predicate_of_symbol symbol,
    boolean_predicate_of_symbol symbol,
    unary_string_predicate_of_symbol context symbol,
    binary_string_predicate_of_symbol context symbol,
    comparison_predicate_of_symbol symbol,
    equality_predicate_of_symbol symbol
  with
  | Some predicate, _, _, _, _, _, _ ->
    unary_value_predicate (context.matches_value_predicate predicate)
  | _, Some predicate, _, _, _, _, _ ->
    unary_value_predicate (context.matches_numeric_predicate predicate)
  | _, _, Some predicate, _, _, _, _ ->
    unary_result_predicate (context.matches_boolean_predicate predicate)
  | _, _, _, Some predicate, _, _, _ ->
    unary_value_predicate (function String value -> predicate value | _ -> false)
  | _, _, _, _, Some predicate, _, _ ->
    binary_string_predicate predicate
  | _, _, _, _, _, Some predicate, _ ->
    (match args with
     | [] -> invalid_arg ("comparison predicate requires at least one argument: " ^ symbol)
     | _ :: _ ->
       clause (fun results ->
         match query_results_as_values context results with
         | Some values -> context.comparison_chain_matches predicate values
         | None -> false))
  | _, _, _, _, _, _, Some predicate ->
    clause (fun results ->
      match query_results_as_values context results with
      | None -> false
      | Some values ->
        let equal = context.all_values_equal values in
        (match predicate with
         | EqualValues -> equal
         | NotEqualValues -> not equal))
  | None, None, None, None, None, None, None ->
    (match symbol, args with
     | ("empty?" | "not-empty" | "not-empty?"), [ _ ] ->
       unary_value_predicate
         (fun value ->
            match symbol with
            | "empty?" -> context.value_has_count 0 value
            | _ -> context.value_is_not_empty value)
     | "contains?", [ _; _ ] ->
       clause (function
         | [ Result_value collection; key_result ] ->
           (match context.value_of_query_result key_result with
            | Some key -> context.value_contains collection key
            | None -> false)
         | _ -> false)
     | "-differ?", _ ->
       clause (fun results ->
         match query_results_as_values context results with
         | None -> false
         | Some values ->
           let left, right = context.split_at (List.length values / 2) values in
           not (List.length left = List.length right && List.for_all2 context.values_equal left right))
     | "identical?", [ _; _ ] ->
       clause (fun results ->
         match query_results_as_values context results with
         | Some [ left; right ] -> context.values_equal left right
         | _ -> false)
     | "identical?", _ -> invalid_arg (two_arg_message symbol)
     | ("empty?" | "not-empty" | "not-empty?"), _ -> invalid_arg (one_arg_message symbol)
     | "contains?", _ -> invalid_arg (two_arg_message symbol)
     | _ -> invalid_arg ("unsupported complement predicate: " ^ symbol))





let parse_join_vars clause_name = function
  | QueryFormVector vars ->
    (match List.map parse_output_var vars with
     | [] -> invalid_arg "Join variables should not be empty"
     | vars -> vars)
  | _ -> invalid_arg (clause_name ^ " join variables must be a vector")

let parse_rule_var = function
  | QueryFormSymbol "_" -> invalid_arg "rule variables must not be placeholders"
  | QueryFormSymbol symbol ->
    if String.length symbol > 1 && symbol.[0] = '?' then
      query_symbol_name symbol
    else
      invalid_arg ("Cannot parse var, expected symbol starting with ?, got: " ^ symbol)
  | _ -> invalid_arg "expected rule variable"

let ensure_distinct_rule_vars clause_name required free =
  let vars = required @ free in
  (if List.length vars <> List.length (List.sort_uniq compare vars) then
    let message =
      match clause_name with
      | "rule" -> "Rule variables should be distinct"
      | _ -> clause_name ^ " rule variables must be distinct"
    in
    invalid_arg message);
  required, free

let parse_rule_vars clause_name = function
  | QueryFormVector ((QueryFormVector required | QueryFormList required) :: free_forms)
  | QueryFormList ((QueryFormVector required | QueryFormList required) :: free_forms) ->
    let required = List.map parse_rule_var required in
    let free = List.map parse_rule_var free_forms in
    (match required, free with
     | [], [] -> invalid_arg "Cannot parse rule-vars"
     | required, free -> ensure_distinct_rule_vars clause_name required free)
  | QueryFormVector free_forms | QueryFormList free_forms ->
    let free = List.map parse_rule_var free_forms in
    (match free with
     | [] -> invalid_arg "Cannot parse rule-vars"
     | free -> ensure_distinct_rule_vars clause_name [] free)
  | _ -> invalid_arg "Cannot parse rule-vars"

let or_join_clause required_vars vars branches =
  match required_vars with
  | [] -> OrJoin (vars, branches)
  | required_vars -> OrJoinRequired (required_vars, vars, branches)

let source_or_join_clause source required_vars vars branches =
  match required_vars with
  | [] -> SourceOrJoin (source, vars, branches)
  | required_vars -> SourceOrJoinRequired (source, required_vars, vars, branches)

let ensure_inferred_join_vars vars =
  if vars = [] then invalid_arg "Join variables should not be empty"

let rec parse_pattern_clause context = function
  | QueryFormVector [ QueryFormList (QueryFormSymbol "missing?" :: args) ] ->
    parse_missing_clause args
  | (QueryFormVector (QueryFormSymbol "not" :: clause_forms)
    | QueryFormList (QueryFormSymbol "not" :: clause_forms)) ->
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "Cannot parse 'not' clause"
     | clauses ->
       ensure_inferred_join_vars (clauses |> List.concat_map vars_of_clause |> List.sort_uniq compare);
       Not clauses)
  | (QueryFormVector (QueryFormSymbol "not-join" :: join_vars :: clause_forms)
    | QueryFormList (QueryFormSymbol "not-join" :: join_vars :: clause_forms)) ->
    let vars = parse_join_vars "not-join" join_vars in
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "Cannot parse 'not-join' clause"
     | clauses -> NotJoin (vars, clauses))
  | (QueryFormVector (QueryFormSymbol "not-join" :: _)
    | QueryFormList (QueryFormSymbol "not-join" :: _)) ->
    invalid_arg "Cannot parse 'not-join' clause"
  | (QueryFormVector (QueryFormSymbol "or" :: branch_forms)
    | QueryFormList (QueryFormSymbol "or" :: branch_forms)) ->
    (match List.map (parse_or_branch context) branch_forms with
     | [] -> invalid_arg "Cannot parse 'or' clause"
     | branches ->
       ensure_inferred_join_vars (branches |> List.concat_map vars_of_branch |> List.sort_uniq compare);
       Or branches)
  | (QueryFormVector (QueryFormSymbol "or-join" :: join_vars :: branch_forms)
    | QueryFormList (QueryFormSymbol "or-join" :: join_vars :: branch_forms)) ->
    let required_vars, vars = parse_rule_vars "or-join" join_vars in
    (match List.map (parse_or_branch context) branch_forms with
     | [] -> invalid_arg "Cannot parse 'or-join' clause"
     | branches -> or_join_clause required_vars vars branches)
  | (QueryFormVector (QueryFormSymbol "or-join" :: _)
    | QueryFormList (QueryFormSymbol "or-join" :: _)) ->
    invalid_arg "Cannot parse 'or-join' clause"
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "not" :: clause_forms)
        | QueryFormList (QueryFormSymbol "not" :: clause_forms))
      ]
    when is_query_source_symbol source_symbol ->
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "source-qualified not requires at least one clause"
     | clauses -> SourceNot (query_source_name source_symbol, clauses))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "not-join" :: join_vars :: clause_forms)
        | QueryFormList (QueryFormSymbol "not-join" :: join_vars :: clause_forms))
      ]
    when is_query_source_symbol source_symbol ->
    let vars = parse_join_vars "source-qualified not-join" join_vars in
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "source-qualified not-join requires at least one clause"
     | clauses -> SourceNotJoin (query_source_name source_symbol, vars, clauses))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "not-join" :: _)
        | QueryFormList (QueryFormSymbol "not-join" :: _))
      ]
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified not-join requires join variables and clauses"
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "or" :: branch_forms)
        | QueryFormList (QueryFormSymbol "or" :: branch_forms))
      ]
    when is_query_source_symbol source_symbol ->
    (match List.map (parse_or_branch context) branch_forms with
     | [] -> invalid_arg "source-qualified or requires at least one branch"
     | branches -> SourceOr (query_source_name source_symbol, branches))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "or-join" :: join_vars :: branch_forms)
        | QueryFormList (QueryFormSymbol "or-join" :: join_vars :: branch_forms))
      ]
    when is_query_source_symbol source_symbol ->
    let required_vars, vars = parse_rule_vars "source-qualified or-join" join_vars in
    (match List.map (parse_or_branch context) branch_forms with
     | [] -> invalid_arg "source-qualified or-join requires at least one branch"
     | branches -> source_or_join_clause (query_source_name source_symbol) required_vars vars branches)
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "or-join" :: _)
        | QueryFormList (QueryFormSymbol "or-join" :: _))
      ]
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified or-join requires join variables and branches"
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "not" :: clause_forms)
    when is_query_source_symbol source_symbol ->
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "source-qualified not requires at least one clause"
     | clauses -> SourceNot (query_source_name source_symbol, clauses))
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "not-join" :: join_vars :: clause_forms)
    when is_query_source_symbol source_symbol ->
    let vars = parse_join_vars "source-qualified not-join" join_vars in
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "source-qualified not-join requires at least one clause"
     | clauses -> SourceNotJoin (query_source_name source_symbol, vars, clauses))
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "not-join" :: _)
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified not-join requires join variables and clauses"
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "or" :: branch_forms)
    when is_query_source_symbol source_symbol ->
    (match List.map (parse_or_branch context) branch_forms with
     | [] -> invalid_arg "source-qualified or requires at least one branch"
     | branches -> SourceOr (query_source_name source_symbol, branches))
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "or-join" :: join_vars :: branch_forms)
    when is_query_source_symbol source_symbol ->
    let required_vars, vars = parse_rule_vars "source-qualified or-join" join_vars in
    (match List.map (parse_or_branch context) branch_forms with
     | [] -> invalid_arg "source-qualified or-join requires at least one branch"
     | branches -> source_or_join_clause (query_source_name source_symbol) required_vars vars branches)
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "or-join" :: _)
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified or-join requires join variables and branches"
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; ((QueryFormVector _ | QueryFormList _) as call)
      ]
    when is_query_source_symbol source_symbol ->
    let source_name = query_source_name source_symbol in
    (match parse_pattern_clause context (QueryFormVector [ call ]) with
     | Rule (rule_name, args) -> SourceRule (source_name, rule_name, args)
     | clause -> SourceClause (source_name, clause))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; ((QueryFormVector _ | QueryFormList _) as call)
      ; output
      ]
    when is_query_source_symbol source_symbol ->
    let source_name = query_source_name source_symbol in
    (match parse_pattern_clause context (QueryFormVector [ call; output ]) with
     | Rule (rule_name, args) -> SourceRule (source_name, rule_name, args)
     | clause -> SourceClause (source_name, clause))
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ]
    when String.length symbol > 1 && symbol.[0] = '?' ->
    DynamicPredicate (query_callable_name symbol, List.map parse_pattern_term args)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ; output
      ]
    when String.length symbol > 1 && symbol.[0] = '?' ->
    (match parse_collection_output_var output with
     | Some output_var ->
       DynamicFunctionCollection (query_callable_name symbol, List.map parse_pattern_term args, output_var)
     | None ->
       (match parse_relation_output_vars output with
        | Some output_vars ->
          DynamicFunctionRelation (query_callable_name symbol, List.map parse_pattern_term args, output_vars)
        | None ->
          DynamicFunction (query_callable_name symbol, List.map parse_pattern_term args, parse_output_vars output)))
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "empty?"; term ]
        | QueryFormVector [ QueryFormSymbol "empty?"; term ])
      ] ->
    EmptyValue (parse_pattern_term term)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol ("not-empty" | "not-empty?"); term ]
        | QueryFormVector [ QueryFormSymbol ("not-empty" | "not-empty?"); term ])
      ] ->
    NotEmptyValue (parse_pattern_term term)
  | QueryFormVector
      [ (QueryFormList
           (QueryFormSymbol (("empty?" | "not-empty" | "not-empty?") as symbol) :: _)
        | QueryFormVector
            (QueryFormSymbol (("empty?" | "not-empty" | "not-empty?") as symbol) :: _))
      ] ->
    invalid_arg ("predicate requires one argument: " ^ symbol)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "contains?"; collection; key ]
        | QueryFormVector [ QueryFormSymbol "contains?"; collection; key ])
      ] ->
    ContainsValue (parse_pattern_term collection, parse_pattern_term key)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "get-else" :: args)
        | QueryFormVector (QueryFormSymbol "get-else" :: args))
      ; QueryFormSymbol output
      ] ->
    parse_get_else_clause args output
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "get-some" :: args)
        | QueryFormVector (QueryFormSymbol "get-some" :: args))
      ; output
      ] ->
    parse_get_some_clause args output
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "get" :: args)
        | QueryFormVector (QueryFormSymbol "get" :: args))
      ; QueryFormSymbol output
      ] ->
    parse_get_clause args output
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "count"; term ]
        | QueryFormVector [ QueryFormSymbol "count"; term ])
      ; QueryFormSymbol output
      ] ->
    CountValue (parse_pattern_term term, query_symbol_name output)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "not"; term ]
        | QueryFormVector [ QueryFormSymbol "not"; term ])
      ; QueryFormSymbol output
      ] ->
    BooleanNotValue (parse_pattern_term term, query_symbol_name output)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "not"; term ]
        | QueryFormVector [ QueryFormSymbol "not"; term ])
      ] ->
    BooleanNotPredicate (parse_pattern_term term)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "not" :: _)
        | QueryFormVector (QueryFormSymbol "not" :: _))
      ] ->
    invalid_arg "not predicate requires one argument"
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "and" :: terms)
        | QueryFormVector (QueryFormSymbol "and" :: terms))
      ] ->
    BooleanAndPredicate (List.map parse_pattern_term terms)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "or" :: terms)
        | QueryFormVector (QueryFormSymbol "or" :: terms))
      ] ->
    BooleanOrPredicate (List.map parse_pattern_term terms)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "-differ?" :: args)
        | QueryFormVector (QueryFormSymbol "-differ?" :: args))
      ] ->
    DifferPredicate (List.map parse_pattern_term args)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "identical?"; left; right ]
        | QueryFormVector [ QueryFormSymbol "identical?"; left; right ])
      ] ->
    IdenticalPredicate (parse_pattern_term left, parse_pattern_term right)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "identical?" :: _)
        | QueryFormVector (QueryFormSymbol "identical?" :: _))
      ] ->
    invalid_arg "identical? requires two arguments"
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "untuple"; tuple ]
        | QueryFormVector [ QueryFormSymbol "untuple"; tuple ])
      ; (QueryFormVector [ QueryFormSymbol output; QueryFormSymbol "..." ]
        | QueryFormList [ QueryFormSymbol output; QueryFormSymbol "..." ])
      ] ->
    GroundTermCollection (parse_pattern_term tuple, query_symbol_name output)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "untuple"; tuple ]
        | QueryFormVector [ QueryFormSymbol "untuple"; tuple ])
      ; output
      ] ->
    UntupleFunction (parse_pattern_term tuple, parse_output_vars output)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "untuple" :: _)
        | QueryFormVector (QueryFormSymbol "untuple" :: _))
      ; _
      ] ->
    invalid_arg "untuple requires one argument"
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "ground" :: args)
        | QueryFormVector (QueryFormSymbol "ground" :: args))
      ; output
      ] ->
    parse_ground_function args output
  | QueryFormVector
      [ (QueryFormList
           ((QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol symbol ]
            | QueryFormVector [ QueryFormSymbol "complement"; QueryFormSymbol symbol ])
            :: args)
        | QueryFormVector
            ((QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol symbol ]
             | QueryFormVector [ QueryFormSymbol "complement"; QueryFormSymbol symbol ])
             :: args))
      ] ->
    parse_complement_predicate_clause context symbol args
  | QueryFormVector
      [ (QueryFormList
           ((QueryFormList (QueryFormSymbol "complement" :: _)
            | QueryFormVector (QueryFormSymbol "complement" :: _))
            :: _)
        | QueryFormVector
            ((QueryFormList (QueryFormSymbol "complement" :: _)
             | QueryFormVector (QueryFormSymbol "complement" :: _))
             :: _))
      ] ->
    invalid_arg "complement requires one predicate symbol"
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "re-find"; pattern; value ]
        | QueryFormVector [ QueryFormSymbol "re-find"; pattern; value ])
      ] ->
    ReFindPredicate (parse_pattern_term pattern, parse_pattern_term value)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "re-matches"; pattern; value ]
        | QueryFormVector [ QueryFormSymbol "re-matches"; pattern; value ])
      ] ->
    ReMatchesPredicate (parse_pattern_term pattern, parse_pattern_term value)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ] ->
    (match
       value_predicate_of_symbol symbol,
       numeric_predicate_of_symbol symbol,
       boolean_predicate_of_symbol symbol,
       unary_string_predicate_clause_of_symbol symbol,
       binary_string_predicate_clause_of_symbol symbol,
       comparison_predicate_of_symbol symbol,
       equality_predicate_of_symbol symbol,
       args
     with
     | Some predicate, _, _, _, _, _, _, [ term ] ->
       ValuePredicate (predicate, parse_pattern_term term)
     | _, Some predicate, _, _, _, _, _, [ term ] ->
       NumericPredicate (predicate, parse_pattern_term term)
     | _, _, Some predicate, _, _, _, _, [ term ] ->
       BooleanPredicate (predicate, parse_pattern_term term)
     | _, _, _, Some clause, _, _, _, [ term ] ->
       clause (parse_pattern_term term)
     | _, _, _, _, Some clause, _, _, [ left; right ] ->
       clause (parse_pattern_term left) (parse_pattern_term right)
     | _, _, _, _, _, Some predicate, _, [ left; right ] ->
       ComparisonPredicate (predicate, parse_pattern_term left, parse_pattern_term right)
     | _, _, _, _, _, Some predicate, _, _ :: _ ->
       ComparisonPredicateN (predicate, List.map parse_pattern_term args)
     | _, _, _, _, _, Some _, _, [] ->
       invalid_arg ("comparison predicate requires at least one argument: " ^ symbol)
     | _, _, _, _, _, _, Some predicate, _ ->
       EqualityPredicate (predicate, List.map parse_pattern_term args)
     | Some _, _, _, _, _, _, _, _
     | _, Some _, _, _, _, _, _, _
     | _, _, Some _, _, _, _, _, _
     | _, _, _, Some _, _, _, _, _
     | _, _, _, _, Some _, _, _, _ ->
       invalid_arg ("predicate requires one argument: " ^ symbol)
     | None, None, None, None, None, None, None, _ ->
       DynamicPredicate (query_callable_name symbol, List.map parse_pattern_term args))
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ; QueryFormSymbol output
      ] ->
    (match arithmetic_op_of_symbol symbol with
     | Some op -> ArithmeticValue (op, List.map parse_pattern_term args, query_symbol_name output)
     | None -> parse_string_transform_function symbol args output)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ; (QueryFormVector _ | QueryFormList _ as output)
      ] ->
    (match parse_collection_output_var output with
     | Some output_var -> parse_collection_value_function symbol args output_var
     | None ->
       (match parse_relation_output_vars output with
        | Some output_vars -> parse_relation_value_function symbol args output_vars
        | None -> parse_flat_value_function symbol args (parse_output_vars output)))
  | (QueryFormVector (QueryFormSymbol rule_name :: args)
    | QueryFormList (QueryFormSymbol rule_name :: args))
    when is_plain_rule_symbol rule_name ->
    let rule_name, args = parse_rule_expr rule_name args in
    Rule (rule_name, args)
  | QueryFormList (QueryFormSymbol source_symbol :: terms) when is_query_source_symbol source_symbol ->
    parse_source_pattern_clause (query_source_name source_symbol) terms
  | QueryFormVector (QueryFormSymbol source_symbol :: terms) when is_query_source_symbol source_symbol ->
    parse_source_pattern_clause (query_source_name source_symbol) terms
  | QueryFormVector terms -> parse_data_pattern_clause terms
  | _ -> invalid_arg "where clauses must be vectors"

and parse_or_branch context = function
  | (QueryFormVector (QueryFormSymbol "and" :: clause_forms)
    | QueryFormList (QueryFormSymbol "and" :: clause_forms)) ->
    (match List.map (parse_pattern_clause context) clause_forms with
     | [] -> invalid_arg "or branch requires at least one clause"
     | clauses -> clauses)
  | form -> [ parse_pattern_clause context form ]

let parse_rule_head = function
  | QueryFormVector (QueryFormSymbol rule_name :: params)
  | QueryFormList (QueryFormSymbol rule_name :: params) ->
    let required_vars, free_vars = parse_rule_vars "rule" (QueryFormVector params) in
    let params = required_vars @ free_vars in
    rule_name, params
  | _ -> invalid_arg "rule head must be a vector or list"

let parse_rule context = function
  | (QueryFormVector (head :: body_forms) | QueryFormList (head :: body_forms)) ->
    let rule_name, rule_params = parse_rule_head head in
    (match List.map (parse_pattern_clause context) body_forms with
     | [] -> invalid_arg "Rule branch should have clauses"
     | rule_body -> { rule_name; rule_params; rule_body })
  | _ -> invalid_arg "rules must be vectors or lists"

let validate_rule_arities rules =
  List.iter
    (fun rule ->
      rules
      |> List.iter (fun other ->
        if rule.rule_name = other.rule_name
           && List.length rule.rule_params <> List.length other.rule_params
        then
          invalid_arg "Arity mismatch"))
    rules;
  rules

let is_rule_head = function
  | QueryFormVector (QueryFormSymbol _ :: _)
  | QueryFormList (QueryFormSymbol _ :: _) -> true
  | _ -> false

let is_rule_form = function
  | QueryFormVector (head :: _)
  | QueryFormList (head :: _) -> is_rule_head head
  | _ -> false

let unwrap_extra_rules_nesting = function
  | [ (QueryFormVector inner | QueryFormList inner) ] when List.for_all is_rule_form inner -> inner
  | rules -> rules

let parse_rules context = function
  | Some (QueryFormVector rules | QueryFormList rules) ->
    unwrap_extra_rules_nesting rules |> List.map (parse_rule context) |> validate_rule_arities
  | Some _ -> invalid_arg "query :rules must be a vector or list"
  | None -> []


let has_rule_clause = Query.has_rule_clause
let rule_names = Query.rule_names
let resolve_dynamic_rule_clause = Query.resolve_dynamic_rule_clause
let resolve_dynamic_rule = Query.resolve_dynamic_rule
let infer_default_inputs = Query.infer_default_inputs
let validate_query = Query.validate_query

let parse_where context = function
  | Some (QueryFormVector clauses | QueryFormList clauses) -> List.map (parse_pattern_clause context) clauses
  | Some _ -> invalid_arg "query :where must be a vector or list"
  | None -> []

let parse_query_return_with_pull_context context ?default_pull_db ?pull_db_for_source form =
  let entries = query_form_map form in
  let return, find =
    parse_find_return
      context
      ~defer_pull_patterns:true
      ?default_pull_db
      ?pull_db_for_source
      (query_form_section "find" entries)
  in
  let in_form = query_form_section "in" entries in
  ensure_distinct_input_rules_var in_form;
  let rules = parse_rules context (query_form_section "rules" entries) in
  let rule_names = rule_names rules in
  let rules = List.map (resolve_dynamic_rule rule_names) rules in
  let where =
    parse_where context (query_form_section "where" entries)
    |> List.map (resolve_dynamic_rule_clause rule_names)
  in
  let inputs = parse_inputs in_form |> infer_default_inputs in_form find where in
  let query =
    { find
    ; inputs
    ; with_vars = parse_with_section (query_form_section "with" entries)
    ; rules
    ; where
    }
    |> validate_query
  in
  if query.rules = []
     && List.exists has_rule_clause query.where
     && not (input_declares_rules_var in_form)
  then invalid_arg "Missing rules var '%' in :in";
  return, query

let parse_query_return context form =
  parse_query_return_with_pull_context context form


let validate_query_return_map = Query.validate_query_return_map

let parse_query_return_map_with_pull_context context ?default_pull_db ?pull_db_for_source form =
  let entries = query_form_map form in
  let return, query =
    parse_query_return_with_pull_context context ?default_pull_db ?pull_db_for_source form
  in
  let return_map = parse_return_map_section entries in
  return, validate_query_return_map return return_map query, query

let parse_query_return_map context form =
  parse_query_return_map_with_pull_context context form

let parse_query context form =
  snd (parse_query_return context form)

let parse_query_with_pull_context context ?default_pull_db ?pull_db_for_source form =
  snd (parse_query_return_with_pull_context context ?default_pull_db ?pull_db_for_source form)

let parse_query_string context input =
  parse_query context (read_edn input)

let parse_query_string_with_pull_context context ?default_pull_db ?pull_db_for_source input =
  parse_query_with_pull_context context ?default_pull_db ?pull_db_for_source (read_edn input)

let parse_query_return_string context input =
  parse_query_return context (read_edn input)

let parse_query_return_string_with_pull_context context ?default_pull_db ?pull_db_for_source input =
  parse_query_return_with_pull_context context ?default_pull_db ?pull_db_for_source (read_edn input)

let parse_query_return_map_string context input =
  parse_query_return_map context (read_edn input)

let parse_query_return_map_string_with_pull_context context ?default_pull_db ?pull_db_for_source input =
  parse_query_return_map_with_pull_context context ?default_pull_db ?pull_db_for_source (read_edn input)
