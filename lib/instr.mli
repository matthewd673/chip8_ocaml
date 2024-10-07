open Stdint

type addr = Uint16.t
type reg = Uint8.t
type byte = Uint8.t
type nibble = Uint8.t (* nibble is 4 bits *)

type t =
  | SysAddr of addr
  | Cls
  | Ret
  | JpAddr of addr
  | CallAddr of addr
  | SeVxByte of reg * byte
  | SneVxByte of reg * byte
  | SeVxVy of reg * reg
  | LdVxByte of reg * byte
  | AddVxByte of reg * byte
  | LdVxVy of reg * reg
  | OrVxVy of reg * reg
  | AndVxVy of reg * reg
  | XorVxVy of reg * reg
  | AddVxVy of reg * reg
  | SubVxVy of reg * reg
  | ShrVxVy of reg * reg
  | SubnVxVy of reg * reg
  | ShlVxVy of reg * reg
  | SneVxVy of reg * reg
  | LdIAddr of addr
  | JpV0Addr of addr
  | RndVxByte of reg * byte
  | DrwVxVyNibble of reg * reg * nibble
  | SkpVx of reg
  | SknpVx of reg
  | LdVxDt of reg
  | LdVxK of reg
  | LdDtVx of reg
  | LdStVx of reg
  | AddIVx of reg
  | LdFVx of reg
  | LdBVx of reg
  | LdIVx of reg
  | LdVxI of reg

  | Mystery
;;

(** [of_u16 u16]
    Parse [u16] to an instruction if it matches any. *)
val of_u16 : Uint16.t -> t

(** [to_string instr]
    Create a string representation of [instr]. *)
val to_string : t -> string
