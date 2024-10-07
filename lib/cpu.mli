type t

(** [create memory display]
    Create a new CPU with [memory] and [display]. *)
val create : Memory.t -> Display.t -> t

(** [tick cpu]
    Tick [cpu] one cycle. *)
val tick : t -> unit
