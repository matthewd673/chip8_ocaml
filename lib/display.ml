open Tsdl

type t = bool Array.t * Sdl.renderer

let width = 64
let height = 32
let len = width * height
let scale = 4

let create renderer =
  (Array.make len false, renderer)
;;

let ind_of_coords x y = y * width + x;;

let coords_of_ind i =
  let x = i mod 64 in
  let y = i / 64 in
  (x, y)
;;

let draw_sprite display sprite x y =
  let (d_arr, _) = display in
  (* TODO: proper drawing *)
  Array.set d_arr (ind_of_coords x y) (Bytes.length sprite <> 0);
  false (* TODO: Return true if any pixels are erased. *)
;;

let clear display =
  let (d_arr, _) = display in
  Array.fill d_arr 0 len false;
;;

let draw_pixel sdl_r i b =
  let (x, y) = coords_of_ind i in
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
