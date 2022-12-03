open Batteries

let input =
  BatFile.lines_of "puzzle-input" |> BatList.of_enum

let char_to_priority = function
  | 'a'..'z' as ch -> (int_of_char ch) - (int_of_char 'a') + 1
  | 'A'..'Z' as ch -> (int_of_char ch) - (int_of_char 'A') + 27
  | _ -> failwith "Unexpected input"

let string_to_priority_set s =
  BatString.explode s |> BatList.map char_to_priority |> BatSet.of_list

let halve_string s =
  let len = (BatString.length s) / 2 in
  (BatString.sub s 0 len, BatString.sub s len len)

let rucksacks =
  BatList.map halve_string input |> BatList.map (BatTuple.Tuple2.mapn string_to_priority_set)

let common_priorities =
  BatList.map (BatSet.any % BatTuple.Tuple2.uncurry BatSet.intersect) rucksacks

let first_puzzle () =
  Printf.printf "First puzzle, the sum of priorities is %d\n" (List.sum common_priorities)

let () =
  print_newline ();
  first_puzzle ();
  ()