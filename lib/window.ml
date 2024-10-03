open Tsdl

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
          (w, r)
;;

let clean_up w =
  Sdl.destroy_window w;
  Sdl.quit ();
;;

let rec loop w last_tick =
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) then
    ()
  else begin
    match Sdl.Event.(enum (get event typ)) with
      | `Quit -> exit 0
      | _ -> ()
  end;
  let ticks = Sdl.get_ticks () |> Int32.to_int in
  let tick_diff = ticks - last_tick in
  if tick_diff <= loop_delay then
    Sdl.delay (Int32.of_int (loop_delay - tick_diff));
  loop w ticks;
;;

let begin_loop w = loop w 0;;
