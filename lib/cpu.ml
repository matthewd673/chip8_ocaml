open Stdint

type t = {
  gp_reg : Uint8.t Array.t;
  mutable reg_I : Uint8.t;
  mutable reg_DT : Uint8.t;
  mutable reg_ST : Uint8.t;
  mutable pc : int;
  mutable sp : int;
  memory : Memory.t;
  display : Display.t;
}

let create memory display =
  { gp_reg = Array.init 16 (fun _ -> Uint8.zero);
    reg_I = Uint8.zero;
    reg_DT = Uint8.zero;
    reg_ST = Uint8.zero;
    pc = 0;
    sp = 0;
    memory = memory;
    display = display;
  }
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
  (* TODO *)
;;
