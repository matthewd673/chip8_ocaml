open Tsdl

val create : unit -> Sdl.window * Sdl.renderer

val clean_up : Sdl.window -> unit

val begin_loop : Sdl.window -> unit
