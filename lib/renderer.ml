open Tsdl

let render sdl_r =
  (* Clear screen *)
  Sdl.set_render_draw_color sdl_r 0 0 0 255 |> ignore;
  Sdl.render_clear sdl_r |> ignore;

  (* TEMP: render a rectangle *)
  let rect = Sdl.Rect.create ~x:50 ~y:50 ~w:20 ~h:20 in
  Sdl.set_render_draw_color sdl_r 255 0 0 255 |> ignore;
  Sdl.render_fill_rect sdl_r (Some rect) |> ignore;

  Sdl.render_present sdl_r;
;;
