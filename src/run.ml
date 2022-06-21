open! Core
open Async
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


let handle_steps game ~game_stopped =
  Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) (fun () ->
    if !game_stopped then ()
    else 
    Mgame.step game;
    Snake_graphics.render game;
    match Mgame.game_state game with
    | Paused | Game_over _ | Win -> (game_stopped := true;)
    | In_progress       -> ())

let pause game ~game_stopped =  
  match Mgame.game_state game with
  (* | Paused -> Mgame.game_state game <- In_progress *)
  | Paused -> 
    (Mgame.set_game_state game In_progress;
    game_stopped := false;)
    (* handle_steps game ~game_over ~game_stopped) *)
  | In_progress -> (Mgame.set_game_state game Paused;
  game_stopped := true;)
  | _ -> () 
;;
  
let handle_keys game ~game_stopped = 
  Clock_ns.every (Time_ns_unix.Span.create ~ns:1 ()) (fun () ->
    match Snake_graphics.read_key () with
    | None     -> ()
    | Some ' ' -> pause game ~game_stopped
    | Some 'r' -> (
      game_stopped := false;
      Mgame.restart game )
    | Some key ->
      Mgame.handle_key game key;
      Snake_graphics.render game; )

let run () =
  let game = Snake_graphics.init_exn () in
  Snake_graphics.render game;
  let game_stopped = ref false in
  handle_keys game  ~game_stopped;
  handle_steps game ~game_stopped;
;;