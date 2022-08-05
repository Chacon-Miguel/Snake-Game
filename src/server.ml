open Core 
open Async

module Constants = struct
  let play_area_height = 400
  let header_height    = 50
  let play_area_width  = 450
  let block_size       = 18
end
 

(* In the implementation for the heartbeat RPC, we need to construct a pipe reader to
    return to the client. In contrast, the implementation for the echo server returned a
    single value. *)

let pause game ~game_stopped =  
  match Mgame.game_state game with
  | Paused -> 
    (Mgame.set_game_state game In_progress;
    game_stopped := false;)
  | In_progress -> (Mgame.set_game_state game Paused;
  game_stopped := true;)
  | _ -> () 
;;

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


(* echo server query *)
let handle_keypress _ (echo : Protocol.Query.echo) = 
  match echo.query_message with
  | ' ' -> (
      pause echo.game ~game_stopped:echo.game_stopped ;
      return ()) (* This is where the game needs to be stopped; add pause*)
  | 'r' -> ( 
      echo.game_stopped := false;
      Mgame.restart echo.game; 
      return ();
  ) (* Restart the game; also add function*)
  | key -> (
    Mgame.handle_key echo.game key; 
    return ();
  )
;;

let run () =
  let open Constants in 
  let height = play_area_height / block_size in 
  let width = play_area_width / block_size in
  Mgame.create ~height ~width ~initial_snake_length:3 
;;
(* heartbeat server query *)
let start_game client { Protocol.Query.frequency_of_heartbeats; player_name} =
  (* create pipe and game *)
  let game_reader, game_writer = Pipe.create () in
  (* let key_reader, key_writer = Pipe.create () in  *)
  let game = ref (run ()) in 
  let game_stopped = ref false in 
  (* send game state at every iteration of every *)
  Clock_ns.every frequency_of_heartbeats (fun () ->
    let message = Protocol.Response.create_hb ~game:!game ~game_stopped in 
    Pipe.write_without_pushback_if_open game_writer message ;
    Mgame.step !game;
    );
    (* handle_keypress *)
  return (Ok game_reader)
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
    [ Rpc.Pipe_rpc.implement Protocol.hb_rpc start_game;
    Rpc.Rpc.implement Protocol.echo_rpc handle_keypress]
;;

let serve port =
  let%bind server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun addr conn ->
        upon (Rpc.Connection.close_finished conn) (fun () ->
          Core.print_s [%message "Client disconnected" (addr : Socket.Address.Inet.t)]);
        addr)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  Tcp.Server.close_finished server
;;

let main =
  let%map_open.Command port =
    flag "-port" (required int) ~doc:"INT port that the server should listen on"
  in
  fun () -> serve port
;;

let command = Command.async ~summary:"start rpc server" main

