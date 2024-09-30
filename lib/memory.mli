type memory

val create : memory

val write_byte : memory -> int -> Char.t -> unit

val write_all : memory -> int -> bytes -> unit

val read_byte : memory -> int -> Char.t

val read_word : memory -> int -> int
