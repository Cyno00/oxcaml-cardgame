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
    printf "\n%s's turn.\n" player_str
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

(** Main game loop with AI *)
let rec game_loop game ~ai_player ~ai_depth =
  display_board game;
  match game.Game_state.decision with
  | Winner _ | Stalemate ->
    display_status game;
    print_endline "\nThanks for playing!"
  | In_progress { whose_turn } ->
    display_status game;
    print_endline (show_legal_moves game);
    if Player_kind.equal whose_turn ai_player
    then (
      (* AI's turn *)
      printf "AI is thinking (depth %d)...\n%!" ai_depth;
      match AI.get_best_move game ~depth:ai_depth with
      | Some move ->
        printf "AI plays column %d\n" move.Move.column;
        (match Game_state.make_move game move with
         | Ok new_game -> game_loop new_game ~ai_player ~ai_depth
         | Error _ ->
           print_endline "AI error!";
           game_loop game ~ai_player ~ai_depth)
      | None ->
        print_endline "AI couldn't find a move!";
        ())
    else (
      (* Human's turn *)
      printf "Enter column (0-6): %!";
      let input = In_channel.input_line In_channel.stdin in
      match input with
      | None -> print_endline "Error reading input"
      | Some line ->
        (match Int.of_string_opt (String.strip line) with
         | None ->
           print_endline "Invalid input. Please enter a number.";
           game_loop game ~ai_player ~ai_depth
         | Some column ->
           (match Game_state.make_move game { Move.column } with
            | Ok new_game -> game_loop new_game ~ai_player ~ai_depth
            | Error Game_is_over ->
              print_endline "Game is already over!";
              game_loop game ~ai_player ~ai_depth
            | Error Column_is_full ->
              print_endline "That column is full! Try another.";
              game_loop game ~ai_player ~ai_depth
            | Error Illegal_column ->
              print_endline "Invalid column! Choose 0-6.";
              game_loop game ~ai_player ~ai_depth)))
;;

(** Entry point *)
let () =
  print_endline "\n========================================";
  print_endline "    Connect Four - Play vs AI!";
  print_endline "========================================\n";
  print_endline "Choose difficulty:";
  print_endline "1. Easy (depth 3)";
  print_endline "2. Medium (depth 5)";
  print_endline "3. Hard (depth 7)";
  printf "\nEnter choice (1-3): %!";
  let difficulty =
    match In_channel.input_line In_channel.stdin with
    | Some "1" -> 3
    | Some "2" -> 5
    | Some "3" -> 7
    | _ ->
      print_endline "Invalid choice, using Medium difficulty.";
      5
  in
  print_endline "\nChoose your color:";
  print_endline "1. Red (you play first)";
  print_endline "2. Yellow (AI plays first)";
  printf "\nEnter choice (1-2): %!";
  let ai_player =
    match In_channel.input_line In_channel.stdin with
    | Some "1" -> Player_kind.Yellow
    | Some "2" -> Player_kind.Red
    | _ ->
      print_endline "Invalid choice, you'll play as Red.";
      Player_kind.Yellow
  in
  print_endline "\nStarting game...\n";
  let game = Game_state.create_standard () in
  game_loop game ~ai_player ~ai_depth:difficulty
;;
