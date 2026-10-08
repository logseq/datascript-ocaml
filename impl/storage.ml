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

let file_storage = Platform.file_storage

let buffered_node_storage pending_entries =
  { PSet.store_node =
      (fun ?address:_ node ->
        (* Addresses are append-only: never write a changed node back to its
           previous address, because older db snapshots keep deferred refs to
           that address and would silently observe the new content
           (a tx-report's db_before then appears to already contain the tx's
           own datoms). Obsolete addresses are reclaimed by collect_garbage. *)
        let address = next_storage_address () in
        pending_entries := (address, Storage_node node) :: !pending_entries;
        address)
  ; restore_node = (fun _address -> None)
  ; accessed = (fun _address -> ())
  }

let normalize_stored_datom_with attr_by_name datom =
  let schema_attr = attr_by_name datom.a in
  match schema_attr, datom.v with
  | Some { value_type = Some RefType; _ }, Int64 entity_id ->
    (match Util.int64_to_int entity_id with
     | Some entity_id -> { datom with v = Ref entity_id }
     | None -> datom)
  | Some { value_type = Some InstantType; _ }, Int64 millis ->
    (* older databases stored plain ints under instant attrs as Instant *)
    { datom with v = Instant millis }
  | Some { value_type = Some InstantType; _ }, Instant _ -> datom
  | Some { value_type = Some TupleType; _ }, Vector values ->
    { datom with v = Tuple (List.map (fun value -> Some value) values) }
  | Some { value_type = Some TupleType; _ }, List values ->
    { datom with v = Tuple (List.map (fun value -> Some value) values) }
  | _ -> datom

let normalize_stored_datom schema =
  normalize_stored_datom_with (Schema.schema_attr_by_name schema)

(* datoms in one node share few attrs; memoize the schema lookup per batch *)
let memoized_attr_by_name schema =
  let memo = Hashtbl.create 16 in
  fun attr ->
    match Hashtbl.find_opt memo attr with
    | Some cached -> cached
    | None ->
        let found = Schema.schema_attr_by_name schema attr in
        Hashtbl.add memo attr found;
        found

let normalize_stored_datoms schema =
  List.map (normalize_stored_datom_with (memoized_attr_by_name schema))

let normalize_stored_node schema =
  let normalize = normalize_stored_datom_with (memoized_attr_by_name schema) in
  function
  | PSet.Leaf datoms -> PSet.Leaf (Array.map normalize datoms)
  | PSet.Branch (keys, child_addresses) ->
    PSet.Branch (Array.map normalize keys, child_addresses)

let normalize_stored_tail schema =
  List.map (normalize_stored_datoms schema)

let restoring_node_storage ?schema storage =
  { PSet.store_node =
      (fun ?address:_ node ->
        (* See buffered_node_storage: writes must be append-only so older
           snapshots' deferred refs to the previous address keep resolving to
           the content they were created with. *)
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

(* cljs datascript's storage root carries :eavt-metadata/:aevt-metadata/
   :avet-metadata ({:count n :shift n}) so that restore-by can rebuild a
   lazy BTSet without reading nodes. shift is the index tree's depth in
   branch levels: a root leaf is 0, a root branch over leaves is 1. *)
let index_metadata pending_entries storage index_set root_address =
  let rec depth address =
    let node =
      match
        List.find_map
          (fun (addr, payload) ->
             match payload with
             | Storage_node node when String.equal addr address -> Some node
             | _ -> None)
          pending_entries
      with
      | Some node -> Some node
      | None ->
          (match storage.storage_restore address with
           | Some (Storage_node node) -> Some node
           | _ -> None)
    in
    match node with
    (* the tree is balanced, so every root-to-leaf path has the same depth:
       descending a single path is enough *)
    | Some (PSet.Branch (_, child_addresses)) when Array.length child_addresses > 0 ->
        1 + depth child_addresses.(0)
    | _ -> 0
  in
  { storage_index_count = PSet.count index_set
  ; storage_index_shift = depth root_address
  }

let root_of_stored_indexes db ~eavt_metadata ~aevt_metadata ~avet_metadata eavt_address aevt_address
    avet_address =
  let settings = PSet.settings db.eavt_index in
  let schema_idents =
    (* eid -> :db/ident pairs, matching cljs's schema map entries *)
    let first_ident = { e = 0; a = "db/ident"; v = Nil; tx = 0; added = true } in
    db.aevt_index
    |> PSet.slice_seq ~from_:first_ident
    |> PSet.to_seq
    |> Seq.take_while (fun d -> String.equal d.a "db/ident")
    |> Seq.filter_map (fun d -> match d.v with Keyword ident -> Some (d.e, ident) | _ -> None)
    |> List.of_seq
  in
  { storage_schema = db.schema
  ; storage_schema_idents = schema_idents
  ; storage_max_eid = db.max_eid
  ; storage_max_tx = db.max_tx
  ; storage_eavt = eavt_address
  ; storage_aevt = aevt_address
  ; storage_avet = avet_address
  ; storage_eavt_metadata = Some eavt_metadata
  ; storage_aevt_metadata = Some aevt_metadata
  ; storage_avet_metadata = Some avet_metadata
  ; storage_duplicate_datoms = db.duplicate_datoms
  ; storage_max_addr = !max_storage_addr
  ; storage_branching_factor = settings.branching_factor
  (* ref-type is an in-memory node-cache policy. Native forces Strong at
     restore (see settings_of_root); don't let that leak into stored
     metadata — keep writing what a JS restore expects. *)
  ; storage_ref_type =
      (if Platform.strong_index_node_cache then PSet.Weak else settings.ref_type)
  }

let stored_settings_of_root root =
  { PSet.branching_factor = root.storage_branching_factor
  ; ref_type = root.storage_ref_type
  }

(* Upstream caches restored index nodes behind js/WeakRef, which survives
   V8 minor GCs; the OCaml GC clears weak slots on every major collection,
   so hot slices keep paying a sqlite reload + transit decode on native.
   Strong refs reproduce the effective upstream cache lifetime there. The
   stored metadata is untouched — this only affects the in-memory cache. *)
let settings_of_root root =
  if Platform.strong_index_node_cache then
    { (stored_settings_of_root root) with PSet.ref_type = PSet.Strong }
  else
    stored_settings_of_root root

let storage_backed_index node_storage index index_set =
  let cmp = Util.compare_datom index in
  let settings = PSet.settings index_set in
  let items = index_set |> PSet.to_list |> Array.of_list in
  PSet.of_sorted_array_by ~settings ~storage:node_storage ~cmp items

let store_index node_storage index index_set =
  match PSet.store index_set with
  | address, _ -> address
  | exception Invalid_argument message when String.equal message "store requires a storage-backed set" ->
    fst (PSet.store (storage_backed_index node_storage index index_set))

(* cljs store-impl! buffers every node write in *store-buffer* and commits a
   single -store batch per tx. The index node storages above used to call
   storage_store once per stored node — Graph_store maps each call to one
   sqlite transaction, so a ~300B node row paid a full commit (~4KB+ WAL)
   instead of ~300B inside a batch.

   Wrap the backend storage so node-only batches accumulate in pending and
   flush together with the first batch carrying the root/tail — the markers
   store_to_storage/store_tail append at the end of every write batch. The
   wrapped record must be the one bound to the conn/db and to the index node
   storages, so all per-node writes within one conn lifetime share the same
   pending buffer; bind it at every entry point below (store, restore,
   restore_root_snapshot, create_conn via empty_db/init_db). Wrapping an
   already wrapped storage stays correct: the inner buffer still flushes on
   the outer flush call. *)
let batch_node_writes (storage : storage) : storage =
  let pending = ref [] in
  { storage with
      storage_store =
        (fun entries ->
          let nodes, rest =
            List.partition
              (fun (_, payload) -> match payload with Storage_node _ -> true | _ -> false)
              entries
          in
          pending := List.rev_append nodes !pending;
          match rest with
          | [] -> ()
          | _ ->
              let batch = List.rev !pending @ rest in
              pending := [];
              storage.storage_store batch)
  ; storage_restore =
      (fun address ->
        match
          List.find_map
            (fun (addr, payload) ->
              if String.equal addr address then Some payload else None)
            !pending
        with
        | Some payload -> Some payload
        | None -> storage.storage_restore address)
  ; storage_delete =
      (fun addresses ->
        pending :=
          List.filter
            (fun (addr, _) -> not (List.mem addr addresses))
            !pending;
        storage.storage_delete addresses)
  }

let store_to_storage db storage =
  let pending_entries = ref [] in
  let node_storage = buffered_node_storage pending_entries in
  let eavt_address = store_index node_storage Eavt db.eavt_index in
  let aevt_address = store_index node_storage Aevt db.aevt_index in
  let avet_address = store_index node_storage Avet db.avet_index in
  let eavt_metadata = index_metadata !pending_entries storage db.eavt_index eavt_address in
  let aevt_metadata = index_metadata !pending_entries storage db.aevt_index aevt_address in
  let avet_metadata = index_metadata !pending_entries storage db.avet_index avet_address in
  let root =
    root_of_stored_indexes db ~eavt_metadata ~aevt_metadata ~avet_metadata eavt_address aevt_address
      avet_address
  in
  storage.storage_store
    (List.rev !pending_entries
     @ [ root_address, Storage_root root
       ; tail_address, Storage_tail []
       ]);
  (* store_node_tree can't write addresses back into immutable nodes, so
     reinstall each index as a lazy Deferred set over the freshly stored
     root — cljs achieves the same by flagging nodes stored in place.
     Rebuild via PSet.restore with the real (readable) node storage:
     the set PSet.store returns inherits whatever storage the input set
     carried, which for sets built by storage_backed_index is this call's
     throwaway buffered storage — adopting it would route later node
     writes into a dead buffer and lose them. *)
  let adopt_node_storage = restoring_node_storage ~schema:db.schema storage in
  let adopt index address metadata =
    match
      PSet.restore ~cmp:(Util.compare_datom index) ~settings:(settings_of_root root)
        ~count:metadata.storage_index_count adopt_node_storage address
    with
    | Some index -> index
    | None -> invalid_arg "stored index failed to restore"
  in
  { db with
      eavt_index = adopt Eavt eavt_address eavt_metadata
    ; aevt_index = adopt Aevt aevt_address aevt_metadata
    ; avet_index = adopt Avet avet_address avet_metadata
    }

let store ?storage db =
  (* The bound storage_ref always wins: it carries the batch buffer the
     index node storages write into. cljs throws when asked to store a db
     to a different IStorage than it was created with; preferring the
     bound one keeps that single-storage invariant. *)
  let storage =
    match db.storage_ref, storage with
    | Some bound, _ -> bound
    | None, Some storage -> batch_node_writes storage
    | None, None -> invalid_arg "db has no attached storage"
  in
  { (store_to_storage db storage) with storage_ref = Some storage }

let store_tail storage tail =
  storage.storage_store [ tail_address, Storage_tail tail ]

(* cljs store-after-transact! compacts the tail once its datom count
   exceeds (:branching-factor (set/settings (:eavt db))) — read the
   branching factor off the db's eavt index, never a constant. *)
let tail_compaction_threshold (db : db) =
  (PSet.settings db.eavt_index).branching_factor

let tail_datom_count tail =
  List.fold_left (fun count group -> count + List.length group) 0 tail

let restore_root_snapshot storage =
  let storage = batch_node_writes storage in
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
  let storage = batch_node_writes storage in
  match storage.storage_restore root_address with
  | None -> None
  | Some (Storage_root root) ->
    note_storage_root root;
    let schema = Schema.validate_schema root.storage_schema in
    let settings = settings_of_root root in
    let node_storage = restoring_node_storage ~schema storage in
    let restore_index index address metadata =
      let cmp = Util.compare_datom index in
      (* Counts and index addresses come from the same stored root
         snapshot. Seed before replaying its separate tail, whose edits
         update counts. Older roots without metadata retain PSS's lazy
         counting fallback. *)
      let count = Option.map (fun metadata -> metadata.storage_index_count) metadata in
      match PSet.restore ?count ~cmp ~settings node_storage address with
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
    let aevt_index = restore_index Aevt root.storage_aevt root.storage_aevt_metadata in
    let avet_index = restore_index Avet root.storage_avet root.storage_avet_metadata in
    let db =
      { db_uid = context.next_db_uid ()
      ; schema
      ; eavt_index = restore_index Eavt root.storage_eavt root.storage_eavt_metadata
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
    address :: List.concat_map (node_addresses storage) (Array.to_list child_addresses)
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
  [ "branching-factor", Int64 (Int64.of_int index_settings.branching_factor)
  ; "ref-type", Keyword (ref_type_keyword index_settings.ref_type)
  ; "storage", Bool (Option.is_some db.storage_ref)
  ]

let collect_garbage storage =
  let live = Hashtbl.create 257 in
  List.iter (fun address -> Hashtbl.replace live address ()) (storage_root_addresses storage);
  storage.storage_list_addresses ()
  |> List.filter (fun address -> not (Hashtbl.mem live address))
  |> storage.storage_delete
