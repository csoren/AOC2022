open Batteries
open Parse


let input = 
  let lines = BatFile.lines_of "puzzle-input" in
  BatEnum.map (fun s -> BatString.split s ~by:" ") lines |> List.of_enum


let value_of_winner = function
  | Me -> 6 | Draw -> 3 | You -> 0

let value_of_hand = function
  | Rock -> 1 | Paper -> 2 | Scissors -> 3

let winner_of_round = function
  | (Scissors, Rock) -> Me
  | (Rock, Scissors) -> You
  | (your_hand, my_hand) ->
      let compare your_score my_score =
        if my_score = your_score then Draw
        else if my_score > your_score then Me
        else You in
      compare (value_of_hand your_hand) (value_of_hand my_hand)

let value_of_round r =
  (winner_of_round r |> value_of_winner) + (snd r |> value_of_hand)

let score_game rounds = 
  BatList.map value_of_round rounds |> BatList.sum

  
let first_puzzle () =
  let rounds = BatList.map round_of_string_pair input in
  let total_score = score_game rounds in
  Printf.printf "First puzzle, my total score is %d\n" total_score


let winning_hand_of = function 
  | Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let losing_hand_of = function 
  | Paper -> Rock | Scissors -> Paper | Rock -> Scissors

let determine_winning_hand = function
  | Me -> winning_hand_of | You -> losing_hand_of | Draw -> Fun.id

let second_puzzle () =
  let outcomes = BatList.map hand_winner_of_string_pair input in
  let rounds = BatList.map (fun (your, winner) -> (your, determine_winning_hand winner your)) outcomes in
  Printf.printf "Second puzzle, my total score is %d\n" (score_game rounds)

  
let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
  