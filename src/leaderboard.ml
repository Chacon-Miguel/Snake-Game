open! Core
open! Async

(* This module is currently entirely unimplemented. It will be implemented in a future
   exercise. *)

type entry = {
  player_name : string
 ; score      : int 
} [@@deriving sexp]

type t = entry list [@@deriving sexp]

(* let file_path = "/home/ubuntu/.snake_scores.sexp" *)
let file_path = "/home/ubuntu/.snake_scores.sexp"

let load () : t Deferred.t =
  Reader.load_sexp_exn file_path t_of_sexp
;;

let compare game1 game2 = 
if game1.score > game2.score then -1
else if game1.score < game2.score then 1
else 0

let update ~player_name ~score () =
  let%bind old_lboard = load () in
  let new_entry = match player_name with 
  | "" -> {player_name = "Player"; score}
  | _ -> {player_name; score} in 
  (* add game recently played to top of the leaderboard *)
  let lboard = new_entry::old_lboard in 
  (* sort the leaderboard *)
  let file_contents = List.sort lboard ~compare:(fun game1 game2 -> compare game1 game2) in 

  let%bind result = Writer.save_sexp file_path (sexp_of_t file_contents) in return file_contents
;;

let to_table t ~n =
  (* to prevent asking for a nonexistant entry *)
  let max = 
    if n > List.length t 
    then List.length t
    else n
  in 
  let rec helper board i result = 
    if i < max then 
      match board with 
      | [] -> ""
      | head::tail -> helper tail (i+1) (result ^ sprintf "%s %d\n" head.player_name head.score)
    else result 
  in helper t 0 "Leader board\n_____________________________\n"
;;
