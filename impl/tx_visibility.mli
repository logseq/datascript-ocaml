open Datascript_types

type view_bounds =
  { view_tx : tx
  ; since_tx : tx option
  ; history : bool
  }

val default_bounds : tx -> view_bounds

val visible_at_tx : view_bounds -> datom -> bool

(** Resolve facts from an ascending datom stream up to [view_bounds].
    When [history] is true, [no_history] attrs still project to current facts only. *)
val apply_view : schema -> view_bounds -> datom list -> datom list

val datoms_filter : datom list -> datom list

val filter_seq : schema -> view_bounds -> datom Seq.t -> datom Seq.t
