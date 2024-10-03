type t

(** [create] Create a new Window. *)
val create : unit -> t

(** [clean_up window] Clean up [window]. *)
val clean_up : t -> unit

(** [begin_loop window] Begin running an event loop for [window]. *)
val begin_loop : t -> unit
