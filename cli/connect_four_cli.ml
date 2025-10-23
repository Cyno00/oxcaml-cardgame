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

(** Display the current game status *)
let display_status game =
  match game.Game_state.decision with
  | In_progress { whose_turn } ->
    let player_str =
      match whose_turn with
      | Red -> "Red (R)"
      | Yellow -> "Yellow (Y)"
    in
    printf "\n%s's turn. Enter column (0-6): %!" player_str
  | Winner player ->
    let player_str =
      match player with
      | Red -> "Red"
      | Yellow -> "Yellow"
    in
    printf "\n🎉 %s wins!\n" player_str
  | Stalemate -> print_endline "\nGame ended in a draw!"
;;

(** Get legal moves as a string *)
let show_legal_moves game =
  let moves = Game_state.get_all_moves game in
  let columns = List.map moves ~f:(fun { Move.column } -> Int.to_string column) in
  "Legal moves: " ^ String.concat ~sep:", " columns
;;

(** Main game loop *)
let rec game_loop game =
  display_board game;
  match game.Game_state.decision with
  | Winner _ | Stalemate ->
    display_status game;
    print_endline "\nThanks for playing!"
  | In_progress _ ->
    print_endline (show_legal_moves game);
    display_status game;
    let input = In_channel.input_line In_channel.stdin in
    (match input with
     | None -> print_endline "Error reading input"
     | Some line ->
       (match Int.of_string_opt (String.strip line) with
        | None ->
          print_endline "Invalid input. Please enter a number.";
          game_loop game
        | Some column ->
          (match Game_state.make_move game { Move.column } with
           | Ok new_game -> game_loop new_game
           | Error Game_is_over ->
             print_endline "Game is already over!";
             game_loop game
           | Error Column_is_full ->
             print_endline "That column is full! Try another.";
             game_loop game
           | Error Illegal_column ->
             print_endline "Invalid column! Choose 0-6.";
             game_loop game)))
;;

(** Entry point *)
let () =
  print_endline "\n========================================";
  print_endline "       Welcome to Connect Four!";
  print_endline "========================================";
  print_endline "\nRed (R) vs Yellow (Y)";
  print_endline "Get 4 in a row to win!\n";
  let game = Game_state.create_standard () in
  game_loop game
;;
