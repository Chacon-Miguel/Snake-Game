open Core 
open Async 

module Query = struct
  (* heartbeat *)
  type hb = { 
    frequency_of_heartbeats : Time_ns_unix.Span.t 
    ; player_name           : string
  }
  [@@deriving sexp_of, bin_io]

  type echo = {
    query_message: char;
    game: Mgame.t ;
    game_stopped: bool ref;
    } [@@deriving sexp_of, bin_io]
  
  let echo_to_string {query_message; game; game_stopped} = query_message
  
  let create_echo ~query_message ~game ~game_stopped = {
    query_message = query_message;
    game = game;
    game_stopped = game_stopped
  }
  
end

module Response = struct 
  type echo = unit [@@deriving sexp_of, bin_io]
  (* you stopped hereeeee *)
  type hb = {
    game: Mgame.t ;
    game_stopped: bool ref
    } [@@deriving sexp_of, bin_io]

  let create_hb ~game ~game_stopped = 
    {
      game = game;
      game_stopped = game_stopped
    }
end

(* echo rpc def *)
let echo_rpc = 
  Rpc.Rpc.create 
  ~name:"send keypress"
  ~version: 0
  ~bin_query: Query.bin_echo
  ~bin_response: Response.bin_echo
;;
(* heartbeat rpc def *)
let hb_rpc =
  Rpc.Pipe_rpc.create
    ~name:"start-sending-game-state"
    ~version:0
    ~bin_query:Query.bin_hb
    ~bin_response:Response.bin_hb
    ~bin_error:Nothing.bin_t
    ()
;;
