type instr

(** [of_u16 u16] parses [u16] to an instruction if it matches any.
    Requires: [u16 >= 0 and u16 <= 65535] *)
val of_u16 : int -> instr

(** [to_string instr] creates a string representation of [instr]. *)
val to_string : instr -> string
