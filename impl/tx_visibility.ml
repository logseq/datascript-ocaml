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

let apply_view bounds datoms =
  let visible = List.filter (visible_at_tx bounds) datoms in
  if bounds.history then visible else datoms_filter visible

let filter_seq bounds seq =
  let datoms =
    Seq.fold_left (fun acc datom -> datom :: acc) [] seq |> List.rev
  in
  apply_view bounds datoms |> List.to_seq
