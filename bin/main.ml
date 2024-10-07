open Chip8

let read_bytes filename =
  let in_ch = open_in filename in
  let buf = Bytes.create 4096 in
  let len = (In_channel.input in_ch buf 0 4096) in
  Bytes.sub buf 0 len (* Return Bytes of exact length that was read *)
;;

let () =
  (* Load data from rom file *)
  let filename = "roms/cavern.ch8" in
  let file_bytes = read_bytes filename in
  Printf.printf "Read %d bytes from %s\n" (Bytes.length file_bytes) filename;

  (* Create SDL resources *)
  let res = Sdl_wrapper.create_res () in
  let (_, sdl_r) = res in

  (* Create emulation components *)
  let memory = Memory.create in
  Memory.write_all memory 0x200 file_bytes;
  let display = Display.create sdl_r in
  let cpu = Cpu.create memory display in

  Sdl_wrapper.begin_loop cpu;
  Sdl_wrapper.clean_up_res res;
;;
