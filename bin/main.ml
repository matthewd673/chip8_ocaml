open Chip8

let read_bytes filename =
  let in_ch = open_in filename in
  let buf = Bytes.create 4096 in
  let len = (In_channel.input in_ch buf 0 4096) in
  Bytes.sub buf 0 len (* Return Bytes of exact length that was read *)
;;

let set_up () =
  (* Load data from rom file *)
  let filename = "roms/cavern.ch8" in
  let file_bytes = read_bytes filename in
  Printf.printf "Read %d bytes from %s\n" (Bytes.length file_bytes) filename;

  (* Create memory and write data into it *)
  let memory = Memory.create in
  Memory.write_all memory 0x200 file_bytes;
;;


let () =
  set_up ();
  let (w, _) = Window.create () in
  Window.begin_loop w;
  Window.clean_up w;
;;
