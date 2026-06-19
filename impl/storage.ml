open Datascript_types

type store_context =
  { serializable : db -> serializable_db
  }

type tail_context =
  { apply_group : db -> datom list -> db
  }

type restore_context =
  { from_serializable : serializable_db -> db
  ; db_with_tail : db -> datom list list -> db
  }

let root_address = "datascript/root"
let tail_address = "datascript/tail"

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
  else Unix.mkdir dir 0o755

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

let store_to_storage context db storage =
  storage.storage_store
    [ root_address, Storage_db (context.serializable db)
    ; tail_address, Storage_tail []
    ]

let store context ?storage db =
  match storage, db.storage_ref with
  | Some storage, _ -> store_to_storage context db storage
  | None, Some storage -> store_to_storage context db storage
  | None, None -> invalid_arg "db has no attached storage"

let store_tail storage tail =
  storage.storage_store [ tail_address, Storage_tail tail ]

let tail_compaction_threshold = 32

let tail_datom_count tail =
  tail |> List.concat |> List.length

let restore_root_snapshot storage =
  match storage.storage_restore root_address with
  | Some (Storage_db snapshot) -> Some snapshot
  | Some (Storage_tail _) -> invalid_arg "storage root does not contain a db"
  | None -> None

let restore_tail_groups storage =
  match storage.storage_restore tail_address with
  | Some (Storage_tail tail) -> tail
  | Some (Storage_db _) -> invalid_arg "storage tail does not contain datom groups"
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
  match restore_root_snapshot storage with
  | None -> None
  | Some snapshot ->
    let db = context.db_with_tail (context.from_serializable snapshot) (restore_tail_groups storage) in
    Some { db with storage_ref = Some storage }

let storage_addresses storage = storage.storage_list_addresses ()

let storage (db : db) = db.storage_ref

let addresses dbs =
  dbs
  |> List.concat_map (fun db ->
    match db.storage_ref with
    | None -> []
    | Some storage -> storage.storage_list_addresses ())
  |> List.sort_uniq compare

let settings (db : db) =
  [ "branching-factor", Int 512
  ; "ref-type", Keyword "soft"
  ; "storage", Bool (Option.is_some db.storage_ref)
  ]

let collect_garbage storage =
  let live =
    [ root_address; tail_address ]
    |> List.filter (fun address -> Option.is_some (storage.storage_restore address))
  in
  storage.storage_list_addresses ()
  |> List.filter (fun address -> not (List.mem address live))
  |> storage.storage_delete
