open Stdint

type addr
type reg
type byte
type nibble

type instr

(** [of_u16 u16]
    Parse [u16] to an instruction if it matches any. *)
val of_u16 : Uint16.t -> instr

(** [to_string instr]
    Create a string representation of [instr]. *)
val to_string : instr -> string
