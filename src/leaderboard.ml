open! Core
open! Async

(* This module is currently entirely unimplemented. It will be implemented in a future
   exercise. *)

type t = unit

let file_path = "/home/ubuntu/.snake_scores.sexp"

let load () : t Deferred.t =
  ignore (file_path : string);
  failwith "Unimplemented"
;;

let update ?player_name ~score () =
  let%bind t = load () in
  ignore (t : t);
  ignore (player_name : string option);
  ignore (score : int);
  failwith "Unimplemented"
;;

let to_table t ~n =
  ignore (t : t);
  ignore (n : int);
  failwith "Unimplemented"
;;
