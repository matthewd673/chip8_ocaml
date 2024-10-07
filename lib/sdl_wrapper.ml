open Tsdl

type res = Sdl.window * Sdl.renderer

let loop_delay = 1000/60;; (* 60fps *)

let create_res () =
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      raise (Failure "Sdl init error")
  | Ok () ->
      match Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.opengl with
      | Error (`Msg e) ->
          Sdl.log "Create window error: %s" e;
          raise (Failure "Sdl create window and renderer failure")
      | Ok (sdl_w, sdl_r) ->
          Sdl.set_window_title sdl_w "chip8_ocaml";
          (sdl_w, sdl_r)
;;

let clean_up_res res =
  let (w, r) = res in
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Sdl.quit ();
;;

let rec loop cpu last_tick =
  (* Handle event. TODO: Avoid re-allocating Event every cycle. *)
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) then
    ()
  else begin
    match Sdl.Event.(enum (get event typ)) with
      | `Quit -> exit 0
      | _ -> ()
  end;

  Cpu.tick cpu;

  (* Wait for next frame *)
  let ticks = Sdl.get_ticks () |> Int32.to_int in
  let tick_diff = ticks - last_tick in
  if tick_diff <= loop_delay then
    Sdl.delay (Int32.of_int (loop_delay - tick_diff));

  loop cpu ticks;
;;

let begin_loop cpu = loop cpu 0;;
