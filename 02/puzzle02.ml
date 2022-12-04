open Batteries
open Model


let input = 
  let lines = File.lines_of "puzzle-input" in
  Enum.map (fun s -> String.split s ~by:" ") lines |> List.of_enum


let value_of_winner = function
  | Me -> 6 | Draw -> 3 | You -> 0

let value_of_hand = function
  | Rock -> 1 | Paper -> 2 | Scissors -> 3

let winner_of_round (you, me) =
  if winning_hand_of you = me then Me
  else if winning_hand_of me = you then You
  else Draw

let value_of_round r =
  (winner_of_round r |> value_of_winner) + (snd r |> value_of_hand)

let score_game rounds = 
  List.map value_of_round rounds |> List.sum


(* First puzzle *)
  
let first_puzzle () =
  let rounds = List.map round_of_string_pair input
  in let total_score = score_game rounds
  in
  Printf.printf "First puzzle, my total score is %d\n" total_score


(* Second puzzle *)

let determine_outcome_hand = function
  | Me -> winning_hand_of | You -> losing_hand_of | Draw -> Fun.id

let second_puzzle () =
  let outcomes = List.map hand_winner_of_string_pair input
  in let rounds = List.map (fun (your, winner) -> (your, determine_outcome_hand winner your)) outcomes
  in
  Printf.printf "Second puzzle, my total score is %d\n" (score_game rounds)

  
let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
  