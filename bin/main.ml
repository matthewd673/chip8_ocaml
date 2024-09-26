open Stdint

type addr = Uint16.t
type reg = Uint8.t
type byte = Uint8.t

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

  | Mystery
;;

let u16_to_parts u16 =
  [u16 land 0xF000 lsr 12;
   u16 land 0x0F00 lsr 8;
   u16 land 0x00F0 lsr 4;
   u16 land 0x000F;
  ]
;;

let rec bytes_to_u16s = function
  | [] -> []
  | a :: b :: t -> ((Char.code a) lsl 8 lor (Char.code b)) :: (bytes_to_u16s t)
  | _ -> raise (Failure ":(")
;;

let combine cast parts =
  let rec aux acc = function
    | [] -> cast acc
    | h :: t -> aux (h lsl 4 lor acc) t
  in
  aux 0 parts
;;

let combine_addr = combine Uint16.of_int
let combine_byte = combine Uint8.of_int

let parse_instr i_u16 =
  match u16_to_parts i_u16 with
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
  | _ -> Mystery (* TODO *)
;;

let instr_to_str = function
  | SysAddr addr -> Printf.sprintf "Sys %s" (Uint16.to_string addr)
  | Cls -> "Cls"
  | Ret -> "Ret"
  | JpAddr addr -> Printf.sprintf "Jp %s" (Uint16.to_string addr)
  | CallAddr addr -> Printf.sprintf "Call %s" (Uint16.to_string addr)
  | SeVxByte (vx, byte) ->
      Printf.sprintf "Se %s %s" (Uint8.to_string vx) (Uint8.to_string byte)
  | SneVxByte (vx, byte) ->
      Printf.sprintf "Sne %s %s" (Uint8.to_string vx) (Uint8.to_string byte)
  | SeVxVy (vx, vy) ->
      Printf.sprintf "Se %s %s" (Uint8.to_string vx) (Uint8.to_string vy)
  | LdVxByte (vx, byte) ->
      Printf.sprintf "Ld %s %s" (Uint8.to_string vx) (Uint8.to_string byte)
  | AddVxByte (vx, byte) ->
      Printf.sprintf "Add %s %s" (Uint8.to_string vx) (Uint8.to_string byte)
  | LdVxVy (vx, vy) ->
      Printf.sprintf "Ld %s %s" (Uint8.to_string vx) (Uint8.to_string vy)
  | Mystery -> "??"
;;

let read_chars filename =
  let in_ch = open_in filename in
  let rec read_all acc =
    match In_channel.input_char in_ch with
      | None -> acc
      | Some h -> read_all (h :: acc)
  in
  let chars = List.rev (read_all []) in
  close_in in_ch;
  chars
;;

let percent_parsed instrs =
  let len = List.length instrs in
  let mysts = List.length
    (List.filter (function Mystery -> true | _ -> false) instrs) in
  (1.0 -. (float_of_int mysts /. float_of_int len)) *. 100.0
;;

let () =
  (* Load data from rom file *)
  let filename = "roms/cavern.ch8" in
  let chars = read_chars filename in
  Printf.printf "Read %d chars from %s\n" (List.length chars) filename;
  let u16s = bytes_to_u16s chars in

  (* Parse and print instructions *)
  let instrs = List.map parse_instr u16s in
  instrs
  |> List.map instr_to_str
  |> List.iter (Printf.printf "%s\t");
  print_endline "";

  (* Just a fun stat... *)
  Printf.printf "%f%% percent parsed\n" (percent_parsed instrs);
;;
