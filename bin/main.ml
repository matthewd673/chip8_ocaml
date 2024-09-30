open Chip8

let rec bytes_to_u16s = function
  | [] -> []
  | a :: b :: t -> ((Char.code a) lsl 8 lor (Char.code b)) :: (bytes_to_u16s t)
  | _ -> raise (Failure ":(")
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

let () =
  (* Load data from rom file *)
  let filename = "roms/cavern.ch8" in
  let chars = read_chars filename in
  Printf.printf "Read %d chars from %s\n" (List.length chars) filename;
  let u16s = bytes_to_u16s chars in

  (* Parse and print instructions *)
  let instrs = List.map Instr.of_u16 u16s in
  instrs
  |> List.map Instr.to_string
  |> List.iter (Printf.printf "%s\t");
  print_endline "";
;;
