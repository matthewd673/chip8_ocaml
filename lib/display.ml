open Tsdl

type t = bool Array.t * Sdl.renderer

let pix_scale = 2

let create renderer =
  (Array.make (64 * 32) false, renderer)
;;

let ind_of_coords x y = y * 64 + x;;

let coords_of_ind i =
  let x = i mod 64 in
  let y = i / 64 in
  (x, y)
;;

let draw_sprite display sprite x y =
  (* TODO: proper drawing *)
  Array.set display (ind_of_coords x y) (Bytes.length sprite <> 0);
;;

let draw_pixel sdl_r i b =
  let (x, y) = Display.coords_of_ind i in
  let rect = Sdl.Rect.create ~x:(x * scale) ~y:(y * scale) ~w:scale ~h:scale in
  if b then
    Sdl.set_render_draw_color sdl_r 255 0 0 255 |> ignore;
  Sdl.render_fill_rect sdl_r (Some rect) |> ignore;
;;

let present display =
  let (d_arr, sdl_r) = display in

  Sdl.set_render_draw_color sdl_r 0 0 0 255 |> ignore;
  Sdl.render_clear sdl_r |> ignore;

  Array.iteri (fun i x -> draw_pixel sdl_r i x) d_arr;

  Sdl.render_present sdl_r;
;;
