open! Core

module Color = struct
  type t = 
  | Red
  | Gold
[@@deriving sexp_of, enumerate, compare, bin_io]
end

type t = { location : Position.t; color: Color.t} [@@deriving sexp_of, bin_io]

let location t = t.location

let color t =  t.color
;;

let amount_to_grow t =
  match color t with
  | Red -> 2
  | Gold -> 3
;;

(* Exercise 05:

   [create] takes in a board and a snake and creates a new apple with a location chosen
   randomly from all possible locations inside the board that are not currently occupied
   by the snake. If there is no location possible it should return [None]. (This will
   happen if the player wins!)

   We have used records before, but hard-coding the apple's location is the first time
   we'll make a new one. We can define a record like so:

   {[
     { field_name1 = value1
     ; field_name2 = value2
     }
   ]}

   As it happens, we may not need to do this any more once we stop hard-coding it.

   We've already written the [create] logic; take a look at the function to make sure you
   understand what it's doing.

   Our implementation of [create] calls [possible_apple_locations], which you'll need to
   implement. It should return the locations on the board where an apple can be generated
   (in any order).

   One function you might find handy for this exercise is [Board.all_locations], which is
   defined in board.ml. [Board.all_locations] takes a board and returns a list of all
   positions on the board.

   Another function that may be useful is [Snake.all_locations], which is defined in
   snake.ml. This function takes a snake and returns all locations it currently occupies.

   Before you get started, check out LIST_FUNCTIONS.mkd for some useful new List
   functions. You probably won't need all of these functions, but they should give you a
   few options for how to write [possible_apple_locations].

   You may feel like your solution to this function is inefficient. That's perfectly fine,
   since the board is quite small. Talk to a TA if you'd like to learn other tools that
   might help make this function more efficient.

   When you're done writing [possible_apple_locations], make sure to check that tests pass
   by running

   $ dune runtest tests/exercise05 *)
let possible_apple_locations ~board ~snake =
  (* ignore board;
  ignore snake; *)
  (* [ { Position.row = 1; col = 1 } ] *)
  List.filter (Board.all_locations board) ~f: (fun x -> 
    not (List.mem (Snake.all_locations snake) x ~equal: (fun pos1 pos2 -> Position.equal pos1 pos2)))
;;

let create ~board ~snake =
  let possible_locations = possible_apple_locations ~board ~snake in
  match (List.random_element possible_locations), List.random_element_exn [1; 2] with
  | None, _          -> None
  | Some location, 1 ->  Some { location; color = Red }
  | Some location, _ -> Some {location; color = Gold}
;;

let create_multiplayer ~board ~snakes =
  let possible_locations = (possible_apple_locations ~board ~snake:snakes.(0))
  @(possible_apple_locations ~board ~snake:snakes.(1)) in
  match (List.random_element possible_locations), List.random_element_exn [1; 2] with
  | None, _          -> None
  | Some location, 1 ->  Some { location; color = Red }
  | Some location, _ -> Some {location; color = Gold}
;;


module Exercises = struct
  let exercise05                    = possible_apple_locations
  let create_with_location location = { location; color = Red}
end
