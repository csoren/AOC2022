type hand = | Rock | Paper | Scissors

let hand_of_string = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "Unexpected input"

let winning_hand_of = function 
  | Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let losing_hand_of = function 
  | Paper -> Rock | Scissors -> Paper | Rock -> Scissors

  
type winner = | Me | You | Draw

let winner_of_string = function
  | "X" -> You
  | "Y" -> Draw
  | "Z" -> Me
  | _ -> failwith "Unexpected input"


let round_of_string_pair (you, me) =
  (hand_of_string you, hand_of_string me)


let hand_winner_of_string_pair (you, winner) =
  (hand_of_string you, winner_of_string winner)
