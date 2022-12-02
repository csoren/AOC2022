open Batteries


type hand = Rock | Paper | Scissors

type winner = | Me | You | Draw


let hand_of_string = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "Unexpected input"


let winner_of_string = function
  | "X" -> You
  | "Y" -> Draw
  | "Z" -> Me
  | _ -> failwith "Unexpected input"


let value_of_hand = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3


let value_of_winner = function
  | Me -> 6
  | Draw -> 3
  | You -> 0


let winner_of_round = function
  | (Scissors, Rock) -> Me
  | (Rock, Scissors) -> You
  | (your_hand, my_hand) ->
      let my_score = value_of_hand my_hand in
      let your_score = value_of_hand your_hand in

      if my_score = your_score then Draw
      else if my_score > your_score then Me
      else You


let value_of_round r =
  (winner_of_round r |> value_of_winner) + (snd r |> value_of_hand)


let round_of_string_pair (you, me) =
  (hand_of_string you, hand_of_string me)


let hand_winner_of_string_pair (you, winner) =
  (hand_of_string you, winner_of_string winner)


let select_winning_hand = function
  | Rock -> Paper | Paper -> Scissors | Scissors -> Rock


let select_losing_hand = function
  | Paper -> Rock | Scissors -> Paper | Rock -> Scissors


let determine_my_winning_hand (your_hand, winner) =
  match winner with
  | Me -> select_winning_hand your_hand
  | You -> select_losing_hand your_hand
  | Draw -> your_hand
    

let string_pairs = 
  let lines = BatFile.lines_of "puzzle-input" in
  BatEnum.map (fun s -> BatString.split s ~by:" ") lines |> List.of_enum


let score_game rounds = 
  BatList.map value_of_round rounds
  |> BatList.sum


let first_puzzle () =
  let rounds = BatList.map round_of_string_pair string_pairs in
  let total_score = score_game rounds in
  Printf.printf "First puzzle, my total score is %d\n" total_score


let second_puzzle () =
  let outcomes = BatList.map hand_winner_of_string_pair string_pairs in
  let rounds = BatList.map (fun hand -> (fst hand, determine_my_winning_hand hand)) outcomes in
  let total_score = score_game rounds in
  Printf.printf "Second puzzle, my total score is %d\n" total_score

  
let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
  
