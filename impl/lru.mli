type ('key, 'value) t
type ('key, 'value) cache

val create : int -> ('key, 'value) t
val assoc : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
val find : 'key -> ('key, 'value) t -> 'value option
val cache : int -> ('key, 'value) cache
val cache_get : ('key, 'value) cache -> 'key -> (unit -> 'value) -> 'value
