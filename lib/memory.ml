type memory = bytes

let create = Bytes.create 4096;;

let write_byte memory addr byte = Bytes.set memory addr byte;;

let write_all memory ind bytes =
  Bytes.blit bytes 0 memory ind (Bytes.length bytes)
;;

let read_byte memory addr = Bytes.get memory addr;;

let read_word memory addr =
  let b1 = Char.code (read_byte memory addr) in
  let b2 = Char.code (read_byte memory (addr + 1)) in
  (b1 lsl 8) lor b2
;;
