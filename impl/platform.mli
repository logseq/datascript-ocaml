type regex

open Datascript_types

(** Return the current wall-clock time as seconds since the Unix epoch. *)
val now_seconds : unit -> float

(** Create a file-backed storage instance rooted at the given path. *)
val file_storage : ?auto_gc:auto_gc -> string -> storage

(** Compile a platform-specific regular expression from a pattern string. *)
val compile_regex : string -> regex

(** Raise [Invalid_argument] when the pattern is not valid JavaScript
    regular-expression syntax, mirroring the eager check [re-pattern]
    performs on the JS platform. *)
val validate_regex : string -> unit

(** Replace regex matches in a string.

    When [first_only] is [true], only the first match is replaced. Otherwise,
    every non-overlapping match is replaced. *)
val replace_regex : first_only:bool -> regex -> string -> string -> string

(** Return the first substring matched by the regex, if any. *)
val regex_find : regex -> string -> string option

(** Return the whole input string when it matches the regex, if any. *)
val regex_matches : regex -> string -> string option

(** Return all non-overlapping substrings matched by the regex. *)
val regex_seq : regex -> string -> string list

(** Split a string around every match of the regex. *)
val split_regex : regex -> string -> string list

(** Split a string around regex matches, producing at most the requested number
    of parts. *)
val split_regex_limited : regex -> string -> int -> string list

(** Whether storage-backed index nodes should be cached with strong
    references. True on native, where the OCaml GC clears weak slots on
    every major collection and would thrash the node cache; false on JS
    runtimes where WeakRef behaves like upstream DataScript. *)
val strong_index_node_cache : bool
