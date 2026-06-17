open Datascript_types

type context =
  { empty_db : unit -> db
  ; q_sources :
      ?inputs:query_arg list ->
      db ->
      (string * query_source) list ->
      query ->
      query_result list list
  ; q_with :
      ?inputs:query_arg list ->
      db ->
      string list ->
      query ->
      query_result list list
  ; parse_query_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query
  ; parse_query_return_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query_return * query
  ; parse_query_return_map_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query_return * query_return_map option * query
  ; compare_value : value -> value -> int
  }

val q : context -> ?inputs:query_arg list -> db -> query -> query_result list list
val q_string : context -> ?inputs:query_arg list -> db -> string -> query_result list list
val q_with :
  context -> ?inputs:query_arg list -> db -> string list -> query -> query_result list list
val q_with_string :
  context -> ?inputs:query_arg list -> db -> string list -> string -> query_result list list
val q_sources :
  context ->
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  query ->
  query_result list list
val q_sources_string :
  context ->
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  string ->
  query_result list list
val q_return : context -> ?inputs:query_arg list -> db -> query_return -> query -> query_output
val q_return_string : context -> ?inputs:query_arg list -> db -> string -> query_output
val q_return_map :
  context ->
  ?inputs:query_arg list ->
  db ->
  query_return ->
  query_return_map ->
  query ->
  query_output
val q_return_map_string : context -> ?inputs:query_arg list -> db -> string -> query_output
