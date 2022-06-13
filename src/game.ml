open! Core

type t =
  { mutable snake      : Snake.t
  ; mutable game_state : Game_state.t
  ; mutable apple      : Apple.t
  ; board              : Board.t
  }
[@@deriving sexp_of]

let to_string { snake; game_state; apple; board } =
  Core.sprintf
    !{|Game state: %{sexp:Game_state.t}
Apple: %{sexp:Apple.t}
Board: %{sexp:Board.t}
Snake:
%s |}
    game_state
    apple
    board
    (Snake.to_string ~indent:2 snake)
;;

let create ~height ~width ~initial_snake_length =
  let board = Board.create ~height ~width               in
  let snake = Snake.create ~length:initial_snake_length in
  let apple = Apple.create ~board ~snake                in
  match apple with
  | None       -> failwith "unable to create initial apple"
  | Some apple ->
    let t = { snake; apple; game_state = In_progress; board } in
    if List.exists (Snake.all_locations snake) ~f:(fun pos ->
      not (Board.in_bounds t.board pos))
    then failwith "unable to create initial snake"
    else t
;;

let snake      t = t.snake
let apple      t = t.apple
let game_state t = t.game_state

let score t =
  ignore t;
  (* Hardcode a score of 0 for now. This may be changed when we implement score-tracking
     in a later exercise. *)
  0
;;

(* Exercise 02b:

   Now, we're going to write a function that will be called whenever the user presses a
   key. For now, the only keys we care about are the ones that should cause the snake to
   change direction.

   To start, let's explore this module a little.

   This module represents the game state. Take a look at the type [t] at the top of this
   file. This is a record definition.

   A record is a data structure that allows you to group several pieces of data together.
   The names of the fields are on the left side of the ':' and the types of those fields
   are on the right side. By default record fields are immutable. The "mutable" keyword
   allows us to modify the value of that field.

   This record has 4 elements: a [snake], a [game_state], an [apple], and a [board]. We'll
   explain each field when it's needed.

   Take a note of the [set_direction] function provided in snake.ml. Given a snake and a
   direction, this function will update the direction stored in the snake.

   Note the signature of this function:
   {[
     val set_direction : t -> Direction.t -> unit
   ]}

   The "unit" type is a special type that is returned by all side-effecting
   functions. This includes behaviors like printing or, as in this function, setting a
   mutable value.

   Recall that we can refer to functions defined in other files by prepending the filename
   (with capitalized first letter) to the function name.

   Let's use the [of_key] function we just wrote in direction.ml to get the direction the
   user intended and set it in the snake.

   The way that you reference the snake field in the record is with a '.' :
   {[
     t.snake
   ]}

   If the key wasn't a valid input key, our [of_key] function will return [None]. In that
   case, we have no action to take. Because we will use a match we will still need to
   specify what action to take in that case. To implement this, we once again use the
   "unit" type. You will probably need the following case in your match statement:
   {[
     | None -> ()
   ]}

   Once you've implemented [handle_key], run

   $ dune runtest ./tests/exercise02b

   You should see no more failures.

   Now if you build and run the game again, you should be able to use the 'w', 'a', 's',
   and 'd' keys to control the snake.

   You may notice weird behavior if you run the snake off the game board. We'll handle
   collision behavior in the next exercise.

   Once you're done, go back to README.mkd for the next exercise.
*)
let handle_key t key =
  ignore t;
  ignore key
;;

(* Exercise 03b:

   Take a look at the definition of the [Game_state.t] type in game_state.mli. Do the
   three variants make sense?

   [check_for_collisions] will be called after the snake has been updated to move forward
   one space. It should check to make sure the snake is still inside the bounds of
   the game board. If the snake is now out of bounds we want to update the game_state
   to note the fact that the game is now over.

   The in_bounds function you wrote in 03a was in the board module, so you can access it
   with [Board.in_bounds].

   If there is a collision we should set the [game_state] of [t] to be
   {[
     Game_over "Out of bounds!"
   ]}

   The way that you set a mutable record value is with the "<-" operator. For example:

   {[
     type t = { mutable counter : int }

     let increment_counter t =
       t.counter <- t.counter + 1
     ;;
   ]}

   [Snake.head] is a function we've provided for you that returns a [Position.t]
   representing the head of the snake.
   {[
     val head : Snake.t -> Position.t
   ]}

   Once you have implemented [check_for_collisions],

   $ dune runtest ./tests/exercise03b

   should have no output.

   Return to README.mkd for instructions on exercise 04.
*)
let check_for_collisions t =
  (* Remember to remove `ignore t` when implemented. *)
  ignore t
;;

(* Exercise 06b:

   Every time the snake steps forward, [maybe_consume_apple] should be called.
   It should check if the snake head is at the current location of the apple
   stored in the game.

   Hint: We've given you some functions to help with this. Take a look at snake.mli and
   apple.mli to find functions to get the locations you need to consider

   If it is, we should call the [grow_over_next_steps] function in snake.ml that we just
   implemented to update the snake so that it can grow over the next few time steps. The
   amount it should grow is based on the value of [Apple.amount_to_grow].

   If the apple is consumed, we should also spawn a new apple on the board using the
   function we encountered in exercise 05, [Apple.create].

   Recall that if [Apple.create] returns [None], that means that we have won the game, so
   we should update the [game_state] to reflect that.

*)
let maybe_consume_apple t =
  (* Remember to remove `ignore t` when implemented. *)
  ignore t
;;

(* Exercise 04b:

   [step] is the function that is called in a loop to make the game progress. As you can
   see, we have provided part of this for you.

   [Snake.step] returns false if the snake collided with itself, and true if the game can
   continue.

   We've already handled the case where the value is true, but when the value is false, we
   currently do nothing.

   Modify this function to set the [game_state] field of the game with the message
   "Self collision!".

   When all the tests for exercise 04 pass, return to README.mkd for exercise 05. *)
let step t =
  if Snake.step t.snake
  then (
    check_for_collisions t;
    maybe_consume_apple t)
  else ()
;;

module Exercises = struct
  let exercise02b = handle_key

  let exercise03b t snake =
    let t = { t with snake } in
    check_for_collisions t;
    t.game_state
  ;;

  let exercise04b t snake =
    let t = { t with snake } in
    step t;
    t.snake, t.game_state
  ;;

  let exercise06b       = maybe_consume_apple
  let set_apple t apple = t.apple <- apple
  let set_snake t snake = t.snake <- snake
end
