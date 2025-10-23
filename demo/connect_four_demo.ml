open! Core
open Tictactoe_logic_library.Connect_four_logic

(** Display a single cell *)
let cell_to_string game row col =
  match Game_state.get_cell game { Cell_position.row; column = col } with
  | Some Red -> "R"
  | Some Yellow -> "Y"
  | None -> "."
;;

(** Display the entire board *)
let display_board game =
  print_endline "\n  0 1 2 3 4 5 6";
  print_endline " +-+-+-+-+-+-+-+";
  for row = 0 to game.Game_state.rows - 1 do
    printf "%d|" row;
    for col = 0 to game.Game_state.columns - 1 do
      printf "%s|" (cell_to_string game row col)
    done;
    print_endline "";
    print_endline " +-+-+-+-+-+-+-+"
  done;
  print_endline ""
;;

(** Display whose turn it is *)
let display_turn game =
  match game.Game_state.decision with
  | In_progress { whose_turn } ->
    let player_str =
      match whose_turn with
      | Red -> "Red (R)"
      | Yellow -> "Yellow (Y)"
    in
    printf "%s's turn\n" player_str
  | Winner player ->
    let player_str =
      match player with
      | Red -> "Red"
      | Yellow -> "Yellow"
    in
    printf "\n🎉 %s WINS!\n\n" player_str
  | Stalemate -> print_endline "\nGame ended in a DRAW!"
;;

(** Play a sequence of moves and display the game *)
let demo_game () =
  print_endline "\n========================================";
  print_endline "    Connect Four Logic Demo";
  print_endline "========================================\n";
  print_endline "Playing a sample game to demonstrate the logic...\n";

  (* This will create a horizontal win for Red *)
  let moves = [ 0; 4; 1; 5; 2; 6; 3 ] in

  let game = ref (Game_state.create_standard ()) in

  print_endline "Initial board:";
  display_board !game;

  List.iteri moves ~f:(fun i column ->
    print_endline (String.make 50 '-');
    printf "Move %d: Playing column %d\n" (i + 1) column;
    display_turn !game;

    match Game_state.make_move !game { Move.column } with
    | Ok new_game ->
      game := new_game;
      display_board !game
    | Error err ->
      printf "Error: %s\n"
        (match err with
         | Game_is_over -> "Game is over"
         | Column_is_full -> "Column is full"
         | Illegal_column -> "Illegal column"));

  display_turn !game;

  print_endline "\n========================================";
  print_endline "Demo complete! Red won with 4 in a row.";
  print_endline "========================================\n"
;;

let () = demo_game ()
