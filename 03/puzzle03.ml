open Batteries

let input =
  BatFile.lines_of "puzzle-input" |> BatList.of_enum

let char_to_priority = function
  | 'a'..'z' as ch -> (int_of_char ch) - (int_of_char 'a') + 1
  | 'A'..'Z' as ch -> (int_of_char ch) - (int_of_char 'A') + 27
  | _ -> failwith "Unexpected input"

let string_to_priority_set s =
  BatString.explode s |> BatList.map char_to_priority |> BatSet.of_list

let common_priorities l =
  BatList.map (BatList.map string_to_priority_set) l
  |> BatList.map (BatSet.any % BatList.reduce BatSet.intersect)


(* First puzzle *)

let halve_string s =
  let len = (BatString.length s) / 2 in
  [BatString.sub s 0 len; BatString.sub s len len]

let rucksacks = BatList.map halve_string input

let first_puzzle () =
  Printf.printf "First puzzle, the sum of priorities is %d\n" (common_priorities rucksacks |> List.sum)


(* Second puzzle *)  

let three_elves = BatList.ntake 3 input

let second_puzzle () =
  Printf.printf "Second puzzle, the sum of priorities is %d\n" (common_priorities three_elves |> List.sum)


let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ();
  ()
