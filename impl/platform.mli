type regex

val now_seconds : unit -> float
val compile_regex : string -> regex
val replace_regex : first_only:bool -> regex -> string -> string -> string
val regex_find : regex -> string -> string option
val regex_matches : regex -> string -> string option
val regex_seq : regex -> string -> string list
val split_regex : regex -> string -> string list
val split_regex_limited : regex -> string -> int -> string list
