open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let bag_successive_ints l =
  List.group_at ~separator:String.is_empty l |> List.map (List.map int_of_string)

let summed_bags =
  bag_successive_ints input |> List.map List.sum

let sorted_bags =
  List.sort (Fun.flip Int.compare) summed_bags

let first_puzzle () =
  Printf.printf "First puzzle, maximum bag of calories is %d\n" (List.hd sorted_bags)

let second_puzzle () = 
  let result = List.take 3 sorted_bags |> List.sum in
  Printf.printf "Second puzzle, sum of top three bags is %d\n" result

let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
