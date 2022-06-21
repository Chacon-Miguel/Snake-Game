open! Core

(* This is the core logic that actually runs the game. We have implemented enough of this
   for you to get started, but feel free to read this file as a reference because you'll
   end up modifying it eventually. *)
let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let handle_steps (game : Mgame.t) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
    Mgame.step game;
    Snake_graphics.render game;
    match Mgame.game_state game with
    | Paused | Game_over _ | Win -> 
      (game_over := true;)
    | In_progress       -> ())
;;

let pause game ~game_over =  
  match Mgame.game_state game with
  (* | Paused -> Mgame.game_state game <- In_progress *)
  | Paused -> 
    (Mgame.set_game_state game In_progress;
    game_over := false;
    handle_steps game ~game_over)
  | In_progress -> Mgame.set_game_state game Paused
  | _ -> () 
;;

let restart game ~game_over =
  match Mgame.game_state game with 
  | Paused
  | In_progress -> Mgame.restart game 
  | Game_over _ 
  | Win ->     (
    Mgame.restart game;
    game_over := false;
    handle_steps game ~game_over)

let handle_keys (game : Mgame.t) ~game_over =
  every ~stop:(ref false) 0.001 ~f:(fun () ->
    match Snake_graphics.read_key () with
    | None     -> ()
    | Some ' ' -> pause game ~game_over
    | Some 'r' -> restart game ~game_over
    | Some key ->
      Mgame.handle_key game key;
      Snake_graphics.render game; )

let run () =
  let game = Snake_graphics.init_exn () in
  Snake_graphics.render game;
  let game_over = ref false in
  handle_keys game ~game_over;
  handle_steps game ~game_over;
;;