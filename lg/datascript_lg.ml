module D = Datascript
module E = Lg_edn_backend

type error = { path : string; message : string }

exception Invalid_data of error

let fail path message = raise (Invalid_data { path; message })

let or_raise = function
  | Ok value -> value
  | Error error -> raise (Invalid_data error)

let error_message error = error.path ^ ": " ^ error.message
let protect f = try Ok (f ()) with Invalid_data error -> Error error

let of_edn value =
  let rec convert path = function
    | E.Nil -> D.QueryFormNil
    | E.Bool value -> D.QueryFormBool value
    | E.String value -> D.QueryFormString value
    | E.Char value -> D.QueryFormInt (Int64.of_int (Uchar.to_int value))
    | E.Symbol value -> D.QueryFormSymbol value
    | E.Keyword value -> D.QueryFormKeyword value
    | E.Small_int value -> D.QueryFormInt (Int64.of_int value)
    | E.Int value -> D.QueryFormInt value
    | E.Bigint value -> tagged "bigint" value
    | E.Decimal value -> tagged "decimal" value
    | E.Ratio value -> tagged "ratio" value
    | E.Regex value -> tagged "regex" value
    | E.Float value -> D.QueryFormFloat value
    | E.List values -> D.QueryFormList (array path values)
    | E.Seq values ->
        D.QueryFormList
          (List.of_seq
             (Seq.mapi
                (fun index value -> convert (at path index) value)
                values))
    | E.Vector values -> D.QueryFormVector (array path values)
    | E.Set values -> D.QueryFormSet (array path values)
    | E.Map entries ->
        D.QueryFormMap
          (Array.to_list
             (Array.mapi
                (fun index (key, value) ->
                  let path = at path index in
                  let key = convert (path ^ ".key") key in
                  let value = convert (path ^ ".value") value in
                  (key, value))
                entries))
    | E.Tagged (tag, value) ->
        D.QueryFormTagged (tag, convert (path ^ ".tagged") value)
    | E.Int_vector values ->
        D.QueryFormVector
          (Array.to_list (Array.map (fun value -> D.QueryFormInt (Int64.of_int value)) values))
    | E.Int4_vector (a, b, value, c) -> packed path a b value c
    | E.Int4_array (a, b, values, c) ->
        let length = Array.length a in
        if
          Array.length b <> length
          || Array.length values <> length
          || Array.length c <> length
        then fail path "packed EDN arrays have different lengths";
        D.QueryFormVector
          (Array.to_list
             (Array.init length (fun index ->
                  packed (at path index) a.(index) b.(index) values.(index)
                    c.(index))))
    | E.Json_source source -> convert path (E.of_json_source source)
  and tagged tag value = D.QueryFormTagged (tag, D.QueryFormString value)
  and at path index = path ^ "[" ^ string_of_int index ^ "]"
  and array path values =
    Array.to_list
      (Array.mapi (fun index value -> convert (at path index) value) values)
  and packed path a b value c =
    D.QueryFormVector
      [
        D.QueryFormInt (Int64.of_int a);
        D.QueryFormInt (Int64.of_int b);
        convert (at path 2) value;
        D.QueryFormInt (Int64.of_int c);
      ]
  in
  try protect (fun () -> convert "$" value)
  with Invalid_argument message | Failure message ->
    Error { path = "$"; message }

let of_edn_exn value = or_raise (of_edn value)
let read source = D.read_edn source
let schema form = D.Data_readers.schema_of_edn_form form

let schema_spec form =
  match
    schema (D.QueryFormMap [ (D.QueryFormKeyword "literal/attribute", form) ])
  with
  | [ (_, spec) ] -> spec
  | _ -> invalid_arg "schema_spec expects one attribute specification"

let tx form = D.Data_readers.tx_data_of_edn_form form
let transact conn form = D.transact_conn conn (tx form)
let entity reference attrs = D.Entity { db_id = reference; attrs }

module Codec = struct
  type 'a t = { encode : 'a -> D.value; decode : D.value -> ('a, error) result }

  let mismatch expected = Error { path = "$"; message = "expected " ^ expected }

  let string =
    {
      encode = (fun value -> D.String value);
      decode = (function D.String value -> Ok value | _ -> mismatch "string");
    }

  let int =
    {
      encode = (fun value -> D.Int64 (Int64.of_int value));
      decode =
        (function
        | D.Int64 value ->
          (match D.Util.int64_to_int value with
           | Some value -> Ok value
           | None -> mismatch "int")
        | _ -> mismatch "int");
    }

  let float =
    {
      encode = (fun value -> D.Float value);
      decode =
        (function
        | D.Float value -> Ok value
        | D.Int64 value -> Ok (Int64.to_float value)
        | _ -> mismatch "float");
    }

  let bool =
    {
      encode = (fun value -> D.Bool value);
      decode = (function D.Bool value -> Ok value | _ -> mismatch "bool");
    }

  let keyword =
    {
      encode = (fun value -> D.Keyword value);
      decode =
        (function D.Keyword value -> Ok value | _ -> mismatch "keyword");
    }

  let entity_id =
    {
      encode = (fun value -> D.Ref value);
      decode = (function D.Ref value -> Ok value | _ -> mismatch "entity ID");
    }

  let value = { encode = Fun.id; decode = (fun value -> Ok value) }
  let agree (_ : 'a t) (_ : 'a t) = ()
  let encode codec value = codec.encode value
  let decode codec value = codec.decode value
end

let at_path path = function
  | Ok value -> Ok value
  | Error error -> Error { error with path = path ^ error.path }

let read_one codec name entity =
  match D.entity_attr entity name with
  | None -> Ok None
  | Some (D.One_value value) ->
      Result.map Option.some (at_path name (Codec.decode codec value))
  | Some _ -> Error { path = name; message = "expected one scalar value" }

module Attribute = struct
  type 'a t = { name : string; codec : 'a Codec.t; spec : D.schema_attr }

  let make name codec spec = { name; codec; spec }
  let codec attribute = attribute.codec
  let codec_for name attribute =
    if name <> attribute.name then invalid_arg "query attribute mapping has a different name";
    attribute.codec
  let name attribute = attribute.name
  let schema attribute = (attribute.name, attribute.spec)

  let entry attribute value =
    if attribute.spec.cardinality <> D.One then
      invalid_arg
        (attribute.name ^ ": use entries for a cardinality-many attribute");
    (attribute.name, D.One_value (Codec.encode attribute.codec value))

  let entries attribute values =
    if attribute.spec.cardinality <> D.Many then
      invalid_arg
        (attribute.name ^ ": use entry for a cardinality-one attribute");
    ( attribute.name,
      D.Many_values (List.map (Codec.encode attribute.codec) values) )

  let read_one attribute entity = read_one attribute.codec attribute.name entity

  let read_many attribute entity =
    match D.entity_attr entity attribute.name with
    | None -> Ok []
    | Some (D.Many_values values) ->
        protect (fun () ->
            List.mapi
              (fun index value ->
                or_raise
                  (at_path
                     (attribute.name ^ "[" ^ string_of_int index ^ "]")
                     (Codec.decode attribute.codec value)))
              values)
    | Some _ ->
        Error
          {
            path = attribute.name;
            message = "expected cardinality-many values";
          }
end

module Projection = struct
  type 'a t = D.query_result list -> ('a, error) result

  let column index codec row =
    if index < 0 then Error { path = "$"; message = "negative query column" }
    else
      match List.nth_opt row index with
      | Some (D.Result_value value) ->
          at_path
            ("column[" ^ string_of_int index ^ "]")
            (Codec.decode codec value)
      | Some (D.Result_entity id) ->
          at_path
            ("column[" ^ string_of_int index ^ "]")
            (Codec.decode codec (D.Ref id))
      | Some (D.Result_attr name) ->
          at_path
            ("column[" ^ string_of_int index ^ "]")
            (Codec.decode codec (D.Keyword name))
      | Some _ ->
          Error
            {
              path = "column[" ^ string_of_int index ^ "]";
              message = "expected a scalar query result";
            }
      | None ->
          Error
            {
              path = "column[" ^ string_of_int index ^ "]";
              message = "missing query column";
            }

  let pair left right row =
    Result.bind (left row) (fun left ->
        Result.map (fun right -> (left, right)) (right row))

  let map f projection row = Result.map f (projection row)
  let run projection row = projection row
end

type 'a query = { parsed : D.query; projection : 'a Projection.t }

let prepare_query projection form = { parsed = D.parse_query form; projection }
let input codec value = D.Arg_scalar (D.Result_value (Codec.encode codec value))

let run_query query db inputs =
  let rows = D.q ~inputs db query.parsed in
  protect (fun () ->
      List.mapi
        (fun index row ->
          or_raise
            (at_path
               ("row[" ^ string_of_int index ^ "]")
               (query.projection row)))
        rows)

module Pull = struct
  type 'a t = D.pulled_entity -> ('a, error) result

  let field attribute entity =
    let name = Attribute.name attribute in
    match List.assoc_opt (D.Keyword name) entity.D.pulled_attrs with
    | None -> Ok None
    | Some (D.Pulled_scalar value) ->
        Result.map Option.some
          (at_path name (Codec.decode attribute.Attribute.codec value))
    | Some _ ->
        Error { path = name; message = "expected one scalar pull value" }

  let pair left right entity =
    Result.bind (left entity) (fun left ->
        Result.map (fun right -> (left, right)) (right entity))

  let map f projection entity = Result.map f (projection entity)
end

let pull db selectors reference projection =
  match D.pull db selectors reference with
  | None -> Ok None
  | Some entity -> Result.map Option.some (projection entity)

let pull_form db form reference projection =
  pull db (D.parse_pull_pattern db form) reference projection
