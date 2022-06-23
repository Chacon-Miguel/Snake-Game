open! Core

(** A [t] represents the entire game state, including the current snake, apple,
    and game state. *)
type t [@@deriving sexp_of]

(** Used for pretty-printing game contents for tests. *)
val to_string  : t -> string

(** [create] creates a new game with specified parameters. *)
val create     : height:int -> width:int -> initial_snake_length:int -> t

(** [snake] returns the snakes that are currently in the game. *)
val snakes      : t -> Snake.t array

(** [handle_key] will be called whenever the user presses a key.  It takes that key and
    updates the game accordingly *)
val handle_key : t -> char -> unit

(** [apple] returns the apple that is currently in the game. *)
val apple      : t -> Apple.t

(** [game_state] returns the state of the current game. *)
val game_state : t -> Game_state.t

val set_game_state : t -> Game_state.t -> unit
val set_snakes     : t -> Snake.t array -> unit
val set_apple      : t -> Apple.t -> unit 
val set_board      : t -> Board.t -> unit
val set_scores     : t -> int array -> unit 

val get_highest_score : t -> int
(** [step] is called in a loop, and the game is re-rendered after each call. *)
val step       : t -> unit

(** [score] returns the game's current scores *)
val scores      : t -> int array

val restart     : t -> unit

(* (** Functions in [Exercises] modules shouldn't be used.  They are only exposed so they
    can be tested *)
module Exercises : sig
  val exercise02b : t -> char -> unit
  val exercise03b : t -> Snake.t -> Game_state.t
  val exercise04b : t -> Snake.t -> Snake.t * Game_state.t
  val exercise06b : t -> unit
  val set_snake   : t -> Snake.t -> unit
  val set_apple   : t -> Apple.t -> unit
end *)
