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
