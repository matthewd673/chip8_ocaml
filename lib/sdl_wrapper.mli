open Tsdl

type res = Sdl.window * Sdl.renderer

(** [create_res] Create new SDL resources. *)
val create_res : unit -> res

(** [clean_up_res res Clean up [res]. *)
val clean_up_res : res -> unit

(** [begin_loop state] Begin running an event loop for [state]. *)
val begin_loop : Cpu.t -> unit
