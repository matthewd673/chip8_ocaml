type t

(** [create memory] Create a new 4KB memory instance. *)
val create : t

(** [write_byte memory addr byte]
    Write [byte] to [memory] at address [addr].
    Requires: [addr] >= 0 and [addr] < 4096. *)
val write_byte : t -> int -> Char.t -> unit

(** [write_all memory ind bytes]
    Blit the entire contents of [bytes] to [memory] starting at address [ind]
    in [memory].
    Requires: [ind] >= 0 and [ind] < 4096. *)
val write_all : t -> int -> bytes -> unit

(** [read_byte memory addr]
    Read the byte at [addr] in [memory].
    Requires: [addr] >= 0 and [addr] < 4096. *)
val read_byte : t -> int -> Char.t

(** [read_word memory addr]
    Read the word at [addr] in [memory].
    Requires: [addr] >= 0 and [addr] < 4096. *)
val read_word : t -> int -> int

(** [read_bytes memory addr n]
    Read [n] bytes in [memory] starting at [addr]. *)
val read_bytes : t -> int -> int -> Bytes.t
