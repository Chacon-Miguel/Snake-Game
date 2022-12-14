open! Core


module Colors = struct
  let black            = Graphics.rgb 000 000 000
  let green            = Graphics.rgb 000 255 000
  let head_color       = Graphics.rgb 100 100 125
  let red              = Graphics.rgb 255 000 000
  let gold             = Graphics.rgb 255 233 0
  let game_in_progress = Graphics.rgb 100 100 200
  let game_lost        = Graphics.rgb 200 100 100
  let game_won         = Graphics.rgb 100 200 100

  let apple_color apple =
    match Apple.color apple with
    | Red -> red
    | Gold -> gold
    ;;
end

module Constants = struct
  let play_area_height = 400
  let header_height    = 50
  let play_area_width  = 450
  let block_size       = 18
end

let only_one : bool ref = ref false

let set_only_one x = only_one := x
;;

let init_exn () =
  let open Constants in
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (play_area_height + header_height) play_area_width);
  let height = play_area_height / block_size in
  let width  = play_area_width  / block_size in
  Mgame.create ~height ~width ~initial_snake_length:3
;;

let initialize_window () = 
  let open Constants in
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (play_area_height + header_height) play_area_width);
;;

(* home screen functions *)
let draw_title () = 
  let open Constants in 
  Graphics.set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let theight, twidth = Graphics.text_size "Snake" in 
  (* let height, width = Graphics.size_x (), Graphics.size_y () in  *)
  (* print_endline (sprintf "%d" height);
  print_endline (sprintf "%d" width); *)
  Graphics.moveto (play_area_width/2 - twidth) (play_area_height/2 - theight);
  Graphics.draw_string ("Snake");
;;

let draw_home_screen () = 
  Graphics.display_mode false;
  let open Constants in
  Graphics.open_graph
    (Printf.sprintf " %dx%d" (play_area_height + header_height) play_area_width);

  draw_title ();

  Graphics.display_mode true;
  Graphics.synchronize  ()
;;

(* Game Board Functions *)
let draw_block { Position.row; col } ~color =
  let open Constants in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color color;
  Graphics.fill_rect (col + 1) (row + 1) (block_size - 1) (block_size - 1)
;;

let draw_header ~game_state ~scores=
  let open Constants in
  let header_color =
    match (game_state : Game_state.t) with
    | In_progress | Paused -> Colors.game_in_progress
    | Game_over _ -> Colors.game_lost
    | Win         -> Colors.game_won
  in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  let header_text = Game_state.to_string game_state in
  Graphics.set_color     Colors.black;
  Graphics.set_text_size 20;
  Graphics.moveto        25 (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf "Player 1 Score: %d" scores.(0));
  Graphics.moveto (play_area_width - 200) (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf "Player 2 Score: %d" scores.(1));
  Graphics.moveto (play_area_width/2 - 100) (play_area_height + 10);
  Graphics.draw_string   (Printf.sprintf " %s" header_text)
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

let draw_apple apple =
  let apple_location = Apple.location apple in
  draw_block apple_location ~color:(Colors.apple_color apple)
;;

let draw_snake snake_head snake_tail =
  List.iter snake_tail ~f:(draw_block ~color:Colors.green);
  (* Snake head is a different color *)
  draw_block ~color:Colors.head_color snake_head
;;

let render game =
  Graphics.display_mode false;
  let snakes = Mgame.snakes game         in
  let apple = Mgame.apple game           in
  let game_state = Mgame.game_state game in
  draw_header ~game_state ~scores: (Mgame.scores game);
  draw_play_area ();
  draw_apple apple;
  draw_snake (Snake.head snakes.(0)) (Snake.tail snakes.(0));
  draw_snake (Snake.head snakes.(1)) (Snake.tail snakes.(1));

  Graphics.display_mode true;
  Graphics.synchronize  ()
;;

let read_key () = if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
