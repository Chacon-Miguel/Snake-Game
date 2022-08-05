open! Core 
open Async

let send_keypress server_addr ~message =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port server_addr)
    (fun connection ->
        Rpc.Rpc.dispatch_exn Protocol.echo_rpc connection message
        )
  >>| Result.ok_exn
;;

(* In the client, rather than getting back a single response from the server, we get
    back a pipe of responses. Here, we iterate over the pipe to print out every new
    update we get from the server. *)
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

let handle_keys_c game server_addr ~game_stopped =
  let pipe = read_keys () in 
  don't_wait_for (
  let%bind next_value = 
    Pipe.read pipe
  in match next_value with 
  | `Eof | `Ok None -> return ()
  | `Ok Some key -> 
    let message = Protocol.Query.create_echo 
    ~query_message: key ~game ~game_stopped in 
    send_keypress server_addr ~message
  (* (String.make 1 key) *)

    (* return ()  *)
  )
;;

let handle_keys_c game server_addr ~game_stopped = 
  (* let process_until_done () =  *)
  let pipe = read_keys () in 
    don't_wait_for (
    let%bind next_value = 
      Pipe.read pipe
    in 
    match next_value with 
    | `Eof -> return ()
    | `Ok None -> return ()
    |`Ok Some key -> (
      (* Mgame.handle_key game key;
      Snake_graphics.render game; *)
      let message = Protocol.Query.create_echo ~query_message:key ~game ~game_stopped in
      send_keypress server_addr ~message ;  
      ))
    (* in Clock_ns.every (Time_ns_unix.Span.create ~ms:100 ()) process_until_done *)
;;

let start_game server_addr ~player_name = 
  Rpc.Connection.with_client
  (Tcp.Where_to_connect.of_host_and_port server_addr)
  (* once connected *)
  (fun connection -> 
    (* open screen where game will be played *)
    Snake_graphics.initialize_window ();
    let%bind.Deferred game_reader, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Protocol.hb_rpc connection 
    (* rate at which game state is streamed *)
    ({ frequency_of_heartbeats = Time_ns_unix.Span.create ~ms:100 () 
      ; player_name = player_name} )
  in 
  Pipe.iter_without_pushback game_reader ~f:(fun message -> 
    Snake_graphics.render message.game;
    handle_keys_c message.game server_addr ~game_stopped:message.game_stopped;
    handle_steps_c player_name message.game ~game_stopped:message.game_stopped;)
  )
  >>| Result.ok_exn
;;

let start_game_command =
  Command.async
    ~summary:"start snake game"
    (let%map_open.Command server_addr =
        flag
          "-server"
          (required host_and_port)
          ~doc:"HOST_AND_PORT server to query (e.g. localhost:1337)"
      and player_name =
        flag "-name" (optional_with_default "" string)
        ~doc:"STRING e.g., Brett" 
      in 
      fun () -> start_game server_addr ~player_name)
;;

let command =
  Command.group
    ~summary:"rpc client"
    [ "start-game", start_game_command]
;;
