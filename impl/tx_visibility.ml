open Datascript_types

(** Upper/lower transaction bounds for a database view. *)
type view_bounds =
  { view_tx : tx
  ; since_tx : tx option
  ; history : bool
  }

let default_bounds max_tx =
  { view_tx = max_tx; since_tx = None; history = false }

let visible_at_tx bounds datom =
  datom.tx <= bounds.view_tx
  &&
  match bounds.since_tx with
  | None -> true
  | Some since_tx -> datom.tx > since_tx

(** Cancel add/retract pairs in ascending index order (dbval `datoms-filter` semantics). *)
let datoms_filter datoms =
  let previous = ref None in
  let result = ref [] in
  let flush_previous () =
    match !previous with
    | None -> ()
    | Some d when d.added -> result := d :: !result
    | Some _ -> ()
  in
  List.iter
    (fun d2 ->
      match !previous with
      | None -> previous := Some d2
      | Some d1 ->
        let same_eav = d1.e = d2.e && d1.a = d2.a && Compare.compare_value d1.v d2.v = 0 in
        if same_eav && d1.added && not d2.added then
          (* later tx retract cancels add *)
          previous := None
        else if same_eav && d1.tx = d2.tx && not d1.added && d2.added then
          (* same-tx retract then add cancels both *)
          previous := None
        else if not d2.added then (
          (* unrelated retract: keep d1 if it was an add, track d2 *)
          if d1.added then result := d1 :: !result;
          previous := Some d2)
        else (
          if d1.added then result := d1 :: !result;
          previous := Some d2))
    datoms;
  flush_previous ();
  List.rev !result

let schema_has_no_history schema attr =
  match List.assoc_opt attr schema with
  | Some { no_history = true; _ } -> true
  | _ -> false

let apply_view schema bounds datoms =
  let visible = List.filter (visible_at_tx bounds) datoms in
  if bounds.history then
    let no_history, historical =
      List.partition (fun d -> schema_has_no_history schema d.a) visible
    in
    datoms_filter no_history @ historical
  else
    datoms_filter visible

(** Streaming cancel for ascending datom sequences (one-datom lookbehind). *)
let datoms_filter_seq seq =
  let previous = ref None in
  let seq_ref = ref seq in
  let rec step () =
    match !seq_ref () with
    | Seq.Nil ->
      (match !previous with
       | Some d when d.added ->
         previous := None;
         Seq.Cons (d, fun () -> Seq.Nil)
       | _ ->
         previous := None;
         Seq.Nil)
    | Seq.Cons (d2, rest) ->
      seq_ref := rest;
      match !previous with
      | None ->
        previous := Some d2;
        step ()
      | Some d1 ->
        let same_eav = d1.e = d2.e && d1.a = d2.a && Compare.compare_value d1.v d2.v = 0 in
        if same_eav && d1.added && not d2.added then (
          previous := None;
          step ())
        else if same_eav && d1.tx = d2.tx && not d1.added && d2.added then (
          previous := None;
          step ())
        else if not d2.added then (
          previous := Some d2;
          if d1.added then Seq.Cons (d1, step) else step ())
        else (
          previous := Some d2;
          if d1.added then Seq.Cons (d1, step) else step ())
  in
  step

let filter_seq schema bounds seq =
  let visible = Seq.filter (visible_at_tx bounds) seq in
  if bounds.history then
    (* History views may keep retracted facts for non-noHistory attrs; materialize
       so noHistory attrs still cancel while historical facts pass through. *)
    let datoms = List.of_seq visible in
    apply_view schema bounds datoms |> List.to_seq
  else
    datoms_filter_seq visible
