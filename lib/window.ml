open Tsdl

type t = {
  sdl_w : Sdl.window;
  sdl_r : Sdl.renderer;
}

let loop_delay = 1000/60;; (* 60fps *)

let create () =
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      raise (Failure "Sdl init error")
  | Ok () ->
      match Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.opengl with
      | Error (`Msg e) ->
          Sdl.log "Create window error: %s" e;
          raise (Failure "Sdl create window and renderer failure")
      | Ok (w, r) ->
          Sdl.set_window_title w "chip8_ocaml";
          { sdl_w = w; sdl_r = r; }
;;

let clean_up window =
  Sdl.destroy_renderer window.sdl_r;
  Sdl.destroy_window window.sdl_w;
  Sdl.quit ();
;;

let rec loop window last_tick =
  (* Handle event *)
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) then
    ()
  else begin
    match Sdl.Event.(enum (get event typ)) with
      | `Quit -> exit 0
      | _ -> ()
  end;

  Renderer.render window.sdl_r;

  (* Wait for next frame *)
  let ticks = Sdl.get_ticks () |> Int32.to_int in
  let tick_diff = ticks - last_tick in
  if tick_diff <= loop_delay then
    Sdl.delay (Int32.of_int (loop_delay - tick_diff));
  loop window ticks;
;;

let begin_loop window = loop window 0;;
