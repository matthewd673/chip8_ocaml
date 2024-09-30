open Stdint

type addr = Uint16.t
type reg = Uint8.t
type byte = Uint8.t
type nibble = Uint8.t (* nibble is 4 bits *)

type instr =
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

let combine cast parts =
  let rec aux acc = function
    | [] -> cast acc
    | h :: t -> aux ((h lsl 4) lor acc) t
  in
  aux 0 parts
;;

let combine_addr = combine Uint16.of_int
let combine_byte = combine Uint8.of_int

let u16_to_parts u16 =
  let u16' = Uint16.to_int u16 in
  [(u16' land 0xF000) lsr 12;
   (u16' land 0x0F00) lsr 8;
   (u16' land 0x00F0) lsr 4;
   (u16' land 0x000F);
  ]
;;

let of_u16 u16 =
  match u16_to_parts u16 with
  | [ 0x0; 0x0; 0xE; 0x0 ] -> Cls
  | [ 0x0; 0x0; 0xE; 0xE ] -> Ret
  | 0x0 :: nnn -> SysAddr (combine_addr nnn)
  | 0x1 :: nnn -> JpAddr (combine_addr nnn)
  | 0x2 :: nnn -> CallAddr (combine_addr nnn)
  | 0x3 :: x :: kk -> SeVxByte (Uint8.of_int x, (combine_byte kk))
  | 0x4 :: x :: kk -> SneVxByte (Uint8.of_int x, (combine_byte kk))
  | [ 0x5; x; y; 0x0 ] -> SeVxVy (Uint8.of_int x, Uint8.of_int y)
  | 0x6 :: x :: kk -> LdVxByte (Uint8.of_int x, (combine_byte kk))
  | 0x7 :: x :: kk -> AddVxByte (Uint8.of_int x, (combine_byte kk))
  | [ 0x8; x; y; 0x0 ] -> LdVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x1 ] -> OrVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x2 ] -> AndVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x3 ] -> XorVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x4 ] -> AddVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x5 ] -> SubVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x6 ] -> ShrVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0x7 ] -> SubnVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x8; x; y; 0xE ] -> ShlVxVy (Uint8.of_int x, Uint8.of_int y)
  | [ 0x9; x; y; 0x0 ] -> SneVxVy (Uint8.of_int x, Uint8.of_int y)
  | 0xA :: nnn -> LdIAddr (combine_addr nnn)
  | 0xB :: nnn -> JpV0Addr (combine_addr nnn)
  | 0xC :: x :: kk -> RndVxByte (Uint8.of_int x, combine_byte kk)
  | [ 0xD; x; y; n ] ->
      DrwVxVyNibble (Uint8.of_int x, Uint8.of_int y, Uint8.of_int n)
  | [ 0xE; x; 0x9; 0xE ] -> SkpVx (Uint8.of_int x)
  | [ 0xE; x; 0xA; 0x1 ] -> SknpVx (Uint8.of_int x)
  | [ 0xF; x; 0x0; 0x7 ] -> LdVxDt (Uint8.of_int x)
  | [ 0xF; x; 0x0; 0xA ] -> LdVxK (Uint8.of_int x)
  | [ 0xF; x; 0x1; 0x5 ] -> LdDtVx (Uint8.of_int x)
  | [ 0xF; x; 0x1; 0x8 ] -> LdStVx (Uint8.of_int x)
  | [ 0xF; x; 0x1; 0xE ] -> AddIVx (Uint8.of_int x)
  | [ 0xF; x; 0x2; 0x9 ] -> LdFVx (Uint8.of_int x)
  | [ 0xF; x; 0x3; 0x3 ] -> LdBVx (Uint8.of_int x)
  | [ 0xF; x; 0x5; 0x5 ] -> LdIVx (Uint8.of_int x)
  | [ 0xF; x; 0x6; 0x5 ] -> LdVxI (Uint8.of_int x)

  | _ -> Mystery (* TODO *)
;;

let fmt_reg reg = Printf.sprintf "V%s" (Uint8.to_string reg);;

let to_string = function
  | SysAddr addr -> Printf.sprintf "Sys %s" (Uint16.to_string addr)
  | Cls -> "Cls"
  | Ret -> "Ret"
  | JpAddr addr -> Printf.sprintf "Jp %s" (Uint16.to_string addr)
  | CallAddr addr -> Printf.sprintf "Call %s" (Uint16.to_string addr)
  | SeVxByte (vx, byte) ->
      Printf.sprintf "Se %s %s" (fmt_reg vx) (Uint8.to_string byte)
  | SneVxByte (vx, byte) ->
      Printf.sprintf "Sne %s %s" (fmt_reg vx) (Uint8.to_string byte)
  | SeVxVy (vx, vy) ->
      Printf.sprintf "Se %s %s" (fmt_reg vx) (fmt_reg vy)
  | LdVxByte (vx, byte) ->
      Printf.sprintf "Ld %s %s" (fmt_reg vx) (Uint8.to_string byte)
  | AddVxByte (vx, byte) ->
      Printf.sprintf "Add %s %s" (fmt_reg vx) (Uint8.to_string byte)
  | LdVxVy (vx, vy) ->
      Printf.sprintf "Ld %s %s" (fmt_reg vx) (fmt_reg vy)
  | OrVxVy (vx, vy) ->
      Printf.sprintf "Or %s %s" (fmt_reg vx) (fmt_reg vy)
  | AndVxVy (vx, vy) ->
      Printf.sprintf "And %s %s" (fmt_reg vx) (fmt_reg vy)
  | XorVxVy (vx, vy) ->
      Printf.sprintf "Xor %s %s" (fmt_reg vx) (fmt_reg vy)
  | AddVxVy (vx, vy) ->
      Printf.sprintf "Add %s %s" (fmt_reg vx) (fmt_reg vy)
  | SubVxVy (vx, vy) ->
      Printf.sprintf "Sub %s %s" (fmt_reg vx) (fmt_reg vy)
  | ShrVxVy (vx, vy) ->
      Printf.sprintf "Shr %s %s" (fmt_reg vx) (fmt_reg vy)
  | SubnVxVy (vx, vy) ->
      Printf.sprintf "Subn %s %s" (fmt_reg vx) (fmt_reg vy)
  | ShlVxVy (vx, vy) ->
      Printf.sprintf "Shl %s %s" (fmt_reg vx) (fmt_reg vy)
  | SneVxVy (vx, vy) ->
      Printf.sprintf "Sne %s %s" (fmt_reg vx) (fmt_reg vy)
  | LdIAddr addr -> Printf.sprintf "Ld I %s" (Uint16.to_string addr)
  | JpV0Addr addr -> Printf.sprintf "Jp V0 %s" (Uint16.to_string addr)
  | RndVxByte (vx, byte) ->
      Printf.sprintf "Rnd %s %s" (fmt_reg vx) (Uint8.to_string byte)
  | DrwVxVyNibble (vx, vy, nibble) ->
      Printf.sprintf
        "Drw %s %s %s" (fmt_reg vx) (fmt_reg vy) (Uint8.to_string nibble)
  | SkpVx vx -> Printf.sprintf "Skp %s" (fmt_reg vx)
  | SknpVx vx -> Printf.sprintf "Sknp %s" (fmt_reg vx)
  | LdVxDt vx -> Printf.sprintf "Ld %s DT" (fmt_reg vx)
  | LdVxK vx -> Printf.sprintf "Ld %s K" (fmt_reg vx)
  | LdDtVx vx -> Printf.sprintf "Ld DT %s" (fmt_reg vx)
  | LdStVx vx -> Printf.sprintf "Ld ST %s" (fmt_reg vx)
  | AddIVx vx -> Printf.sprintf "Add I %s" (fmt_reg vx)
  | LdFVx vx -> Printf.sprintf "Ld F %s" (fmt_reg vx)
  | LdBVx vx -> Printf.sprintf "Ld B %s" (fmt_reg vx)
  | LdIVx vx -> Printf.sprintf "Ld I %s" (fmt_reg vx)
  | LdVxI vx -> Printf.sprintf "Ld %s I" (fmt_reg vx)

  | Mystery -> "??"
;;

