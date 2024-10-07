open Tsdl

type t

(** [create renderer] Create a new display with the given SDL renderer. *)
val create : Sdl.renderer -> t

(** [draw_sprite display sprite x y]
    Draw the given sprite data at the [x] and [y] coordinates of [display].
    Drawing will be XOR'd as expected by the Drw instruction.
    Return: [true] if any pixels are erased. [false] otherwise. *)
val draw_sprite : t -> bytes -> int -> int -> bool

(** [clear display]
    Set all pixels to "off" on [display]. *)
val clear : t -> unit

(** [present display]
    Present the contents of [display] on the display's SDL renderer.
    This method not only draws the contents of [display] to the renderer but
    also calls [Sdl.render_present] on the renderer. *)
val present : t -> unit
