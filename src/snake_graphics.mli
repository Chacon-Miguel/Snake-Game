open! Core

(** This module handles the graphics for the game. We have implemented this for
    you so you don't need to change anything here, but feel free to look around
    and once you have the game, feel free to alter this file to make things
    fancier! *)

(** [init_exn] fails if called twice. *)
val init_exn : unit -> Mgame.t

(** [render] renders the entire playing area along with snakes and apples. *)
val render : Mgame.t -> unit

(** [read_key] returns a keyboard input, if it's available. *)
val read_key : unit -> char option

val set_only_one : bool -> unit

val draw_home_screen : unit -> unit 

val initialize_window : unit -> unit