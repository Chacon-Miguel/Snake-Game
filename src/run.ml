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

let handle_steps player_name game ~game_stopped =
  Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) (fun () ->
    if !game_stopped then ()
    else 
    Mgame.step game;
    Snake_graphics.render game;
    match Mgame.game_state game with
    | Paused  -> (game_stopped := true;)
    | Game_over _ | Win -> (
      if phys_equal !game_stopped false
      then (
      game_stopped := true;  
      upon (Leaderboard.update ~player_name ~score: (Mgame.get_highest_score game) ()) 
      (fun x -> print_endline (Leaderboard.to_table x ~n:10) );
      ))
    | In_progress       -> ())

let handle_steps_c player_name game ~game_stopped = (fun () ->
  if !game_stopped then ()
  else 
  Mgame.step game;
  Snake_graphics.render game;
  match Mgame.game_state game with
  | Paused  -> (game_stopped := true;)
  | Game_over _ | Win -> (
    if phys_equal !game_stopped false
    then (
    game_stopped := true;  
    upon (Leaderboard.update ~player_name ~score: (Mgame.get_highest_score game) ()) 
    (fun x -> print_endline (Leaderboard.to_table x ~n:10) );
    ))
  | In_progress       -> ())
;;
  

let pause game ~game_stopped =  
  match Mgame.game_state game with
  | Paused -> 
    (Mgame.set_game_state game In_progress;
    game_stopped := false;)
  | In_progress -> (Mgame.set_game_state game Paused;
  game_stopped := true;)
  | _ -> () 
;;

let read_keys () = 
  let r, w = Pipe.create() in 
  let write_key_into_pipe () = don't_wait_for (Pipe.write w (Snake_graphics.read_key ())) in 
   (Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) 
  (write_key_into_pipe ));
  r
;;

let handle_keys game ~game_stopped = 
  let process_until_done () = 
  let pipe = read_keys () in 
    don't_wait_for (
    let%bind next_value = 
      Pipe.read pipe
    in 
    match next_value with 
    | `Eof -> return ()
    | `Ok None -> (); Deferred.return ()
    | `Ok Some ' ' ->  (
      pause game ~game_stopped; Deferred.return ())
    | `Ok Some 'r' -> (
      game_stopped := false;
      Mgame.restart game; Deferred.return ())
    |`Ok Some key -> (
      Mgame.handle_key game key;
      Snake_graphics.render game;
      Deferred.return ()  ))
  in Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) process_until_done
;;

let handle_keys_c game ~game_stopped = 
  (* let process_until_done () =  *)
    let pipe = read_keys () in 
      don't_wait_for (
      let%bind next_value = 
        Pipe.read pipe
      in 
      match next_value with 
      | `Eof -> return ()
      | `Ok None -> (); Deferred.return ()
      | `Ok Some ' ' ->  (
        pause game ~game_stopped; Deferred.return ())
      | `Ok Some 'r' -> (
        game_stopped := false;
        Mgame.restart game; Deferred.return ())
      |`Ok Some key -> (
        Mgame.handle_key game key;
        Snake_graphics.render game;
        Deferred.return ()  ))
    (* in Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) process_until_done *)
  ;;

(* let handle_keys game ~game_stopped = 
  Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) (fun () ->
    match Snake_graphics.read_key () with
    | None     -> ()
    | Some ' ' -> pause game ~game_stopped
    | Some 'r' -> (
      game_stopped := false;
      Mgame.restart game )
    | Some key ->
      Mgame.handle_key game key;
      Snake_graphics.render game; ) *)

(* let run () =
  let game = Snake_graphics.init_exn () in
  Snake_graphics.render game;
  let game_stopped = ref false in
  handle_keys game  ~game_stopped;
  handle_steps game ~game_stopped;
;; *)
(* let command =
  Command.basic
    ~summary:"Snake game"
    [%map_open.Command
      let player_name  = flag "-name" (optional_with_default "" string ) 
      ~doc:"STRING e.g., Brett" in 
      fun () ->
        let game = Snake_graphics.init_exn () in
        Snake_graphics.render game;
        let game_stopped = ref false in
        handle_keys game  ~game_stopped;
        handle_steps player_name game ~game_stopped;
        (* Snake_graphics.draw_home_screen (); *)
       ] *)
    let command = 
      Command.group
      ~summary: "server and client test"
      ["server", Server.command; "client", Client.command]
;;
(* let run_command () = Command_unix.run command *)