open Batteries

let input =
  BatFile.lines_of "puzzle-input" |> BatList.of_enum

let char_to_priority = function
  | 'a'..'z' as ch -> (int_of_char ch) - (int_of_char 'a') + 1
  | 'A'..'Z' as ch -> (int_of_char ch) - (int_of_char 'A') + 27
  | _ -> failwith "Unexpected input"

let common_items =
  BatList.map (BatList.map (String.explode %> BatSet.of_list))
  %> BatList.map (BatList.reduce BatSet.intersect %> BatSet.any)

let solve = common_items %> BatList.map char_to_priority %> BatList.sum


(* First puzzle *)

let halve_string s =
  let len = (BatString.length s) / 2 in
  [BatString.sub s 0 len; BatString.sub s len len]

let first_puzzle () =
  let rucksacks = BatList.map halve_string input in
  Printf.printf "First puzzle, the sum of priorities is %d\n" (solve rucksacks)


(* Second puzzle *)  

let second_puzzle () =
  let groups_of_three_elves = BatList.ntake 3 input in
  Printf.printf "Second puzzle, the sum of priorities is %d\n" (solve groups_of_three_elves)


let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
