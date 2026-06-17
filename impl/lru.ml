type ('key, 'value) entry =
  { key : 'key
  ; value : 'value
  ; gen : int
  }

type ('key, 'value) t =
  { limit : int
  ; gen : int
  ; entries : ('key, 'value) entry list
  }

type ('key, 'value) cache =
  { mutable state : ('key, 'value) t
  }

let create limit =
  if limit < 0 then invalid_arg "LRU limit must not be negative";
  { limit; gen = 0; entries = [] }

let find key lru =
  lru.entries
  |> List.find_opt (fun entry -> entry.key = key)
  |> Option.map (fun entry -> entry.value)

let cleanup (lru : ('key, 'value) t) =
  let newest_entries =
    lru.entries
    |> List.sort (fun (left : ('key, 'value) entry) right -> compare right.gen left.gen)
  in
  let rec take count entries =
    if count <= 0 then []
    else
      match entries with
      | [] -> []
      | entry :: rest -> entry :: take (count - 1) rest
  in
  { lru with entries = take lru.limit newest_entries }

let assoc key value lru =
  let value = Option.value (find key lru) ~default:value in
  let entries = List.filter (fun entry -> entry.key <> key) lru.entries in
  cleanup { lru with gen = lru.gen + 1; entries = { key; value; gen = lru.gen } :: entries }

let cache limit = { state = create limit }

let cache_get cache key compute =
  match find key cache.state with
  | Some value ->
    cache.state <- assoc key value cache.state;
    value
  | None ->
    let value = compute () in
    cache.state <- assoc key value cache.state;
    value
