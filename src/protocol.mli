open Core 
open Async 
module Query : sig
    (* configure heartbeats for streaming game state *)
    type hb = { 
    frequency_of_heartbeats : Time_ns_unix.Span.t
    ; player_name           : string } 
    [@@deriving sexp_of]
    (* sent when user presses key *)
    type echo = {
        query_message: char;
        game: Mgame.t ;
        game_stopped: bool ref;
        }
    [@@deriving sexp_of]

    val echo_to_string : echo -> char

    val create_echo    : query_message:char -> game:Mgame.t -> game_stopped:bool ref -> echo 
end

module Response : sig 
    type echo = unit [@@deriving sexp_of, bin_io]

    type hb = {
        game: Mgame.t  ;
        game_stopped: bool ref
        }[@@deriving sexp_of, bin_io]

    val create_hb : game:Mgame.t  -> game_stopped:bool ref -> hb
end 
(* Unlike the echo server's [rpc], which is of type [Rpc.Rpc.t], this RPC is a
    [Rpc.Pipe_rpc.t]. For the echo server, a single query from the client will yield a
    single response from the server. With pipe RPC, the heartbeat server will be able to
    stream events to the client. *)
val echo_rpc : (Query.echo, Response.echo) Rpc.Rpc.t 

val hb_rpc : (Query.hb, Response.hb, Nothing.t) Rpc.Pipe_rpc.t
