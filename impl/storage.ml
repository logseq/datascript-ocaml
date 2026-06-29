open Datascript_types

module PSet = Persistent_sorted_set

type tail_context =
  { apply_group : db -> datom list -> db
  }

type restore_context =
  { next_db_uid : unit -> int
  ; db_with_tail : db -> datom list list -> db
  }

let root_address = "0"
let tail_address = "1"

let max_storage_addr = ref 1_000_000

let next_storage_address () =
  incr max_storage_addr;
  string_of_int !max_storage_addr

let note_storage_root root =
  max_storage_addr := max !max_storage_addr root.storage_max_addr

let memory_storage () =
  let disk = ref [] in
  let store entries =
    disk :=
      List.fold_left
        (fun disk (address, payload) -> (address, payload) :: List.remove_assoc address disk)
        !disk
        entries
  in
  let restore address = List.assoc_opt address !disk in
  let list_addresses () =
    !disk
    |> List.map fst
    |> List.sort_uniq compare
  in
  let delete addresses =
    disk := List.filter (fun (address, _) -> not (List.mem address addresses)) !disk
  in
  { storage_store = store
  ; storage_restore = restore
  ; storage_list_addresses = list_addresses
  ; storage_delete = delete
  }

let ensure_storage_dir dir =
  if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      invalid_arg ("storage path is not a directory: " ^ dir)
  end
  else Sys.mkdir dir 0o755

let hex_digit value =
  Char.chr (if value < 10 then Char.code '0' + value else Char.code 'a' + value - 10)

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> Char.code ch - Char.code 'a' + 10
  | 'A' .. 'F' as ch -> Char.code ch - Char.code 'A' + 10
  | ch -> invalid_arg ("invalid storage address hex digit: " ^ String.make 1 ch)

let encode_storage_address address =
  String.init
    (String.length address * 2)
    (fun index ->
      let code = Char.code address.[index / 2] in
      if index mod 2 = 0 then hex_digit (code lsr 4) else hex_digit (code land 0x0f))

let decode_storage_address encoded =
  if String.length encoded mod 2 <> 0 then
    invalid_arg ("invalid storage address filename: " ^ encoded);
  String.init
    (String.length encoded / 2)
    (fun index ->
      let high = hex_value encoded.[index * 2] in
      let low = hex_value encoded.[index * 2 + 1] in
      Char.chr ((high lsl 4) lor low))

let storage_payload_path dir address =
  Filename.concat dir (encode_storage_address address ^ ".bin")

let file_storage dir =
  ensure_storage_dir dir;
  let write_payload address payload =
    let channel = open_out_bin (storage_payload_path dir address) in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> Marshal.to_channel channel payload [])
  in
  let read_payload address =
    let path = storage_payload_path dir address in
    if not (Sys.file_exists path) then None
    else
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> Some (Marshal.from_channel channel : storage_payload))
  in
  let list_addresses () =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter_map (fun filename ->
      if Filename.extension filename = ".bin" then
        let base = Filename.remove_extension filename in
        Some (decode_storage_address base)
      else
        None)
    |> List.sort_uniq compare
  in
  let delete addresses =
    List.iter
      (fun address ->
        let path = storage_payload_path dir address in
        if Sys.file_exists path then Sys.remove path)
      addresses
  in
  { storage_store =
      (fun entries ->
        List.iter (fun (address, payload) -> write_payload address payload) entries)
  ; storage_restore = read_payload
  ; storage_list_addresses = list_addresses
  ; storage_delete = delete
  }

let buffered_node_storage pending_entries =
  { PSet.store_node =
      (fun node ->
        let address = next_storage_address () in
        pending_entries := (address, Storage_node node) :: !pending_entries;
        address)
  ; restore_node = (fun _address -> None)
  ; accessed = (fun _address -> ())
  }

let normalize_stored_datom schema datom =
  let schema_attr = Schema.schema_attr_by_name schema datom.a in
  match schema_attr, datom.v with
  | Some { value_type = Some RefType; _ }, Int entity_id -> { datom with v = Ref entity_id }
  | Some { value_type = Some TupleType; _ }, Vector values ->
    { datom with v = Tuple (List.map (fun value -> Some value) values) }
  | Some { value_type = Some TupleType; _ }, List values ->
    { datom with v = Tuple (List.map (fun value -> Some value) values) }
  | _ -> datom

let normalize_stored_datoms schema =
  List.map (normalize_stored_datom schema)

let normalize_stored_node schema = function
  | PSet.Leaf datoms -> PSet.Leaf (normalize_stored_datoms schema datoms)
  | PSet.Branch (keys, child_addresses) ->
    PSet.Branch (normalize_stored_datoms schema keys, child_addresses)

let normalize_stored_tail schema =
  List.map (normalize_stored_datoms schema)

let restoring_node_storage ?schema storage =
  { PSet.store_node =
      (fun node ->
        let address = next_storage_address () in
        storage.storage_store [ address, Storage_node node ];
        address)
  ; restore_node =
      (fun address ->
        match storage.storage_restore address with
        | Some (Storage_node node) ->
          Some
            (match schema with
             | None -> node
             | Some schema -> normalize_stored_node schema node)
        | Some _ -> invalid_arg ("storage node address does not contain a node: " ^ address)
        | None -> None)
  ; accessed = (fun _address -> ())
  }

let root_of_stored_indexes db eavt_address aevt_address avet_address =
  let settings = PSet.settings db.eavt_index in
  { storage_schema = db.schema
  ; storage_max_eid = db.max_eid
  ; storage_max_tx = db.max_tx
  ; storage_eavt = eavt_address
  ; storage_aevt = aevt_address
  ; storage_avet = avet_address
  ; storage_duplicate_datoms = db.duplicate_datoms
  ; storage_max_addr = !max_storage_addr
  ; storage_branching_factor = settings.branching_factor
  ; storage_ref_type = settings.ref_type
  }

let settings_of_root root =
  { PSet.branching_factor = root.storage_branching_factor
  ; ref_type = root.storage_ref_type
  }

let storage_backed_index node_storage index index_set =
  let cmp = Util.compare_datom index in
  let settings = PSet.settings index_set in
  let items = index_set |> PSet.to_list |> Array.of_list in
  PSet.of_sorted_array_by ~settings ~storage:node_storage ~cmp items

let store_index node_storage index index_set =
  match PSet.store index_set with
  | address, _ -> address
  | exception Invalid_argument message when String.equal message "store requires a storage-backed set" ->
    let storage_backed = storage_backed_index node_storage index index_set in
    fst (PSet.store storage_backed)

let store_to_storage db storage =
  let pending_entries = ref [] in
  let node_storage = buffered_node_storage pending_entries in
  let eavt_address = store_index node_storage Eavt db.eavt_index in
  let aevt_address = store_index node_storage Aevt db.aevt_index in
  let avet_address = store_index node_storage Avet db.avet_index in
  let root = root_of_stored_indexes db eavt_address aevt_address avet_address in
  storage.storage_store
    (List.rev !pending_entries
     @ [ root_address, Storage_root root
       ; tail_address, Storage_tail []
       ])

let store ?storage db =
  match storage, db.storage_ref with
  | Some storage, _ -> store_to_storage db storage
  | None, Some storage -> store_to_storage db storage
  | None, None -> invalid_arg "db has no attached storage"

let store_tail storage tail =
  storage.storage_store [ tail_address, Storage_tail tail ]

let tail_compaction_threshold = 32

let tail_datom_count tail =
  tail |> List.concat |> List.length

let restore_root_snapshot storage =
  match storage.storage_restore root_address with
  | Some (Storage_root root) ->
    note_storage_root root;
    let schema = Schema.validate_schema root.storage_schema in
    let settings = settings_of_root root in
    let node_storage = restoring_node_storage ~schema storage in
    (match PSet.restore ~cmp:(Util.compare_datom Eavt) ~settings node_storage root.storage_eavt with
     | Some eavt ->
       Some
         { serializable_schema = root.storage_schema
         ; serializable_datoms =
             PSet.to_list eavt @ normalize_stored_datoms schema root.storage_duplicate_datoms
             |> List.sort (Util.compare_datom Eavt)
         ; serializable_max_eid = root.storage_max_eid
         ; serializable_max_tx = root.storage_max_tx
         }
     | None -> invalid_arg ("storage root points at a missing index: " ^ root.storage_eavt))
  | Some (Storage_tail _) -> invalid_arg "storage root does not contain a db"
  | Some (Storage_node _) -> invalid_arg "storage root does not contain root metadata"
  | None -> None

let restore_tail_groups storage =
  match storage.storage_restore tail_address with
  | Some (Storage_tail tail) -> tail
  | Some (Storage_root _) | Some (Storage_node _) -> invalid_arg "storage tail does not contain datom groups"
  | None -> []

let db_with_tail context db tail =
  List.fold_left
    (fun db group ->
      match group with
      | [] -> db
      | first :: _ ->
        let group_tx = first.tx in
        let db_before_group = { db with max_tx = group_tx - 1 } in
        let db_after_group =
          match context.apply_group db_before_group group with
          | db -> db
          | exception Invalid_argument _ -> db_before_group
        in
        { db_after_group with max_tx = group_tx })
    db
    tail

let restore context storage =
  match storage.storage_restore root_address with
  | None -> None
  | Some (Storage_root root) ->
    note_storage_root root;
    let schema = Schema.validate_schema root.storage_schema in
    let settings = settings_of_root root in
    let node_storage = restoring_node_storage ~schema storage in
    let restore_index index address =
      match PSet.restore ~cmp:(Util.compare_datom index) ~settings node_storage address with
      | Some index -> index
      | None -> invalid_arg ("storage root points at a missing index: " ^ address)
    in
    let duplicate_datoms = normalize_stored_datoms schema root.storage_duplicate_datoms in
    let duplicate_eavt_by_entity =
      let table = Hashtbl.create 1024 in
      List.iter
        (fun datom ->
          let existing = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
          Hashtbl.replace table datom.e (datom :: existing))
        duplicate_datoms;
      Hashtbl.iter (fun entity_id datoms -> Hashtbl.replace table entity_id (List.rev datoms)) table;
      table
    in
    let duplicate_datoms_by_attr duplicate_datoms =
      let table = Hashtbl.create 1024 in
      List.iter
        (fun datom ->
          let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
          Hashtbl.replace table datom.a (datom :: existing))
        duplicate_datoms;
      Hashtbl.iter (fun attr datoms -> Hashtbl.replace table attr (List.rev datoms)) table;
      table
    in
    let duplicate_aevt_datoms = List.sort (Util.compare_datom Aevt) duplicate_datoms in
    let duplicate_avet_datoms =
      duplicate_datoms
      |> List.filter (fun datom -> Schema.schema_attr_is_avet_accessible schema datom.a)
      |> List.sort (Util.compare_datom Avet)
    in
    let aevt_index = restore_index Aevt root.storage_aevt in
    let avet_index = restore_index Avet root.storage_avet in
    let db =
      { db_uid = context.next_db_uid ()
      ; schema
      ; eavt_index = restore_index Eavt root.storage_eavt
      ; aevt_index
      ; avet_index
      ; aevt_by_attr = Hashtbl.create 0
      ; avet_by_attr = Hashtbl.create 0
      ; duplicate_datoms
      ; duplicate_aevt_datoms
      ; duplicate_avet_datoms
      ; duplicate_eavt_by_entity
      ; duplicate_aevt_by_attr = duplicate_datoms_by_attr duplicate_aevt_datoms
      ; duplicate_avet_by_attr = duplicate_datoms_by_attr duplicate_avet_datoms
      ; max_eid = root.storage_max_eid
      ; max_datom_e = root.storage_max_eid
      ; max_tx = root.storage_max_tx
      ; filter_pred = None
      ; storage_ref = Some storage
      ; tx_fns = []
      }
    in
    Some (context.db_with_tail db (normalize_stored_tail schema (restore_tail_groups storage)))
  | Some (Storage_tail _) -> invalid_arg "storage root does not contain root metadata"
  | Some (Storage_node _) -> invalid_arg "storage root does not contain root metadata"

let storage_addresses storage = storage.storage_list_addresses ()

let storage (db : db) = db.storage_ref

let rec node_addresses storage address =
  match storage.storage_restore address with
  | Some (Storage_node (PSet.Leaf _)) -> [ address ]
  | Some (Storage_node (PSet.Branch (_, child_addresses))) ->
    address :: List.concat_map (node_addresses storage) child_addresses
  | Some _ -> [ address ]
  | None -> []

let storage_root_addresses storage =
  match storage.storage_restore root_address with
  | Some (Storage_root root) ->
    [ root_address; tail_address ]
    @ node_addresses storage root.storage_eavt
    @ node_addresses storage root.storage_aevt
    @ node_addresses storage root.storage_avet
  | Some (Storage_tail _) | Some (Storage_node _) | None ->
    []

let addresses dbs =
  dbs
  |> List.concat_map (fun db ->
    match db.storage_ref with
    | None -> []
    | Some storage -> storage_root_addresses storage)
  |> List.sort_uniq compare

let ref_type_keyword = function
  | PSet.Strong -> "strong"
  | PSet.Weak -> "weak"

let settings (db : db) =
  let index_settings = PSet.settings db.eavt_index in
  [ "branching-factor", Int index_settings.branching_factor
  ; "ref-type", Keyword (ref_type_keyword index_settings.ref_type)
  ; "storage", Bool (Option.is_some db.storage_ref)
  ]

let collect_garbage storage =
  let live = storage_root_addresses storage in
  storage.storage_list_addresses ()
  |> List.filter (fun address -> not (List.mem address live))
  |> storage.storage_delete
