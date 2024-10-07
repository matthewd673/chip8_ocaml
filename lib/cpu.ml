open Stdint

include Instr

type t = {
  gp_reg : Uint8.t Array.t;
  mutable reg_I : Uint16.t;
  mutable reg_DT : Uint8.t;
  mutable reg_ST : Uint8.t;
  mutable pc : int;
  mutable sp : int;
  stack : int Array.t;
  memory : Memory.t;
  display : Display.t;
}

let create memory display =
  { gp_reg = Array.init 16 (fun _ -> Uint8.zero);
    reg_I = Uint16.zero;
    reg_DT = Uint8.zero;
    reg_ST = Uint8.zero;
    pc = 0;
    sp = 0;
    stack = Array.make 16 0;
    memory = memory;
    display = display;
  }
;;

let reg_VF = Uint8.of_int 0xF;;

let inc_pc cpu = cpu.pc <- cpu.pc + 2;;
let inc_sp cpu = cpu.sp <- cpu.sp + 1;;
let dec_sp cpu = cpu.sp <- cpu.sp - 1;;

let get_reg cpu x = Array.get cpu.gp_reg (Uint8.to_int x);;
let set_reg cpu x v = Array.set cpu.gp_reg (Uint8.to_int x) v;;

let get_F cpu f = Uint16.zero;; (* TODO *)

let interpret_instr cpu = function
  | SysAddr _ -> () (* apparently ignored by modern interpreters *)
  | Cls -> Display.clear cpu.display
  | Ret -> begin
    cpu.pc <- Array.get cpu.stack cpu.sp;
    dec_sp cpu;
    end
  | JpAddr addr -> cpu.pc <- Uint16.to_int addr
  | CallAddr addr -> begin
    inc_sp cpu;
    Array.set cpu.stack cpu.sp cpu.pc;
    cpu.pc <- Uint16.to_int addr;
    end
  | SeVxByte (vx, kk) -> if vx = kk then inc_pc cpu;
  | SneVxByte (vx, kk) -> if vx <> kk then inc_pc cpu;
  | SeVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    if x_val = y_val then inc_pc cpu;
    end
  | LdVxByte (vx, kk) -> set_reg cpu vx kk;
  | AddVxByte (vx, kk) -> begin
    let x_val = get_reg cpu vx in
    set_reg cpu vx (Uint8.add x_val kk); (* NOTE: using `+` gives type error. *)
    end
  | LdVxVy (vx, vy) -> begin
    let y_val = get_reg cpu vy in
    set_reg cpu vx y_val;
    end
  | OrVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    set_reg cpu vx (Uint8.logor x_val y_val);
    end
  | AndVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    set_reg cpu vx (Uint8.logand x_val y_val);
    end
  | XorVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    set_reg cpu vx (Uint8.logxor x_val y_val);
    end
  | AddVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    set_reg cpu reg_VF
      (if (Uint8.to_int x_val) + (Uint8.to_int y_val) > 255
        then Uint8.one
        else Uint8.zero);
    set_reg cpu vx (Uint8.add x_val y_val);
    end
  | SubVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    set_reg cpu reg_VF (if x_val > y_val then Uint8.one else Uint8.zero);
    set_reg cpu vx (Uint8.sub x_val y_val);
    end
  | ShrVxVy (vx, _) -> begin (* TODO: what is the point of Vy? *)
    let x_val = get_reg cpu vx in
    set_reg cpu reg_VF
      (if (Uint8.logand x_val (Uint8.one)) <> Uint8.zero
        then Uint8.one else Uint8.zero);
    set_reg cpu vx (Uint8.shift_right_logical x_val 1);
    end
  | SubnVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    set_reg cpu reg_VF (if y_val > x_val then Uint8.one else Uint8.zero);
    set_reg cpu vx (Uint8.sub y_val x_val);
    end
  | ShlVxVy (vx, _) -> begin
    let x_val = get_reg cpu vx in
    set_reg cpu reg_VF
      (if (Uint8.logand x_val (Uint8.of_int 0x8)) <> Uint8.zero
        then Uint8.one else Uint8.zero);
    set_reg cpu vx (Uint8.shift_left x_val 1);
    end
  | SneVxVy (vx, vy) -> begin
    let x_val = get_reg cpu vx in
    let y_val = get_reg cpu vy in
    if x_val <> y_val then inc_pc cpu;
    end
  | LdIAddr nnn -> cpu.reg_I <- nnn;
  | JpV0Addr addr -> begin
    let v0_val = get_reg cpu Uint8.zero in
    let sum = (Uint16.to_int addr) + (Uint8.to_int v0_val) in
    cpu.pc <- sum;
    end
  | RndVxByte (vx, kk) -> begin
    let rnd = Uint8.of_int (Random.int 256) in
    let prod = (Uint8.logand rnd kk) in
    set_reg cpu vx prod;
    end
  | DrwVxVyNibble (vx, vy, nibble) -> begin
    let x_val = Uint8.to_int (get_reg cpu vx) in
    let y_val = Uint8.to_int (get_reg cpu vy) in
    let sprite = Memory.read_bytes
      cpu.memory
      (Uint16.to_int cpu.reg_I)
      (Uint8.to_int nibble) in
    set_reg cpu reg_VF
      (if Display.draw_sprite cpu.display sprite x_val y_val
        then Uint8.one else Uint8.zero);
    end
  | SkpVx _ -> raise (Failure "Not implemented") (* TODO *)
  | SknpVx _ -> raise (Failure "Not implemented") (* TODO *)
  | LdVxDt vx -> set_reg cpu vx cpu.reg_DT;
  | LdVxK _ -> raise (Failure "Not implemented") (* TODO *)
  | LdDtVx vx -> cpu.reg_DT <- get_reg cpu vx;
  | LdStVx vx -> cpu.reg_ST <- get_reg cpu vx;
  | AddIVx vx -> begin
    let x_val = get_reg cpu vx in
    cpu.reg_I <- Uint16.add cpu.reg_I (Uint16.of_uint8 x_val);
    end
  | LdFVx vx -> begin
    let x_val = get_reg cpu vx in
    cpu.reg_I <- get_F cpu x_val;
    end
  | LdBVx _ -> raise (Failure "Not implemented") (* TODO *)
  | LdIVx _ -> raise (Failure "Not implemented") (* TODO *)
  | LdVxI _ -> raise (Failure "Not implemented") (* TODO *)
  | Mystery -> raise (Failure "Not an instr");
;;

let tick_dt cpu =
  if Uint8.to_int cpu.reg_DT > 0 then
    cpu.reg_DT <- Uint8.sub cpu.reg_DT Uint8.one;
;;

let tick_st cpu =
  if Uint8.to_int cpu.reg_ST > 0 then
    cpu.reg_ST <- Uint8.sub cpu.reg_ST Uint8.one;
;;

let tick cpu =
  tick_dt cpu;
  tick_st cpu;

  (* Read and interpret next instr *)
  Memory.read_word cpu.memory cpu.pc
    |> Uint16.of_int
    |> Instr.of_u16
    |> interpret_instr cpu
;;
