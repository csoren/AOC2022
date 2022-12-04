open Batteries

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let char_to_priority = function
  | 'a'..'z' as ch -> (int_of_char ch) - (int_of_char 'a') + 1
  | 'A'..'Z' as ch -> (int_of_char ch) - (int_of_char 'A') + 27
  | _ -> failwith "Unexpected input"

let common_items =
  List.map (List.map (String.explode %> Set.of_list))
  %> List.map (List.reduce Set.intersect %> Set.any)

let solve = common_items %> List.map char_to_priority %> List.sum


(* First puzzle *)

let halve_string s =
  let len = (String.length s) / 2 in
  [String.sub s 0 len; String.sub s len len]

let first_puzzle () =
  let rucksacks = List.map halve_string input in
  Printf.printf "First puzzle, the sum of priorities is %d\n" (solve rucksacks)


(* Second puzzle *)  

let second_puzzle () =
  let groups_of_three_elves = List.ntake 3 input in
  Printf.printf "Second puzzle, the sum of priorities is %d\n" (solve groups_of_three_elves)


let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
