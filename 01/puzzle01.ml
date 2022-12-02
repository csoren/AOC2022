open Batteries

let bag_successive_ints l = 
  BatEnum.group (not % BatString.is_empty) l
  |> BatEnum.map (BatEnum.filter (not % BatString.is_empty))
  |> BatEnum.map (BatEnum.map int_of_string)

let summed_bags =
  let lines = BatFile.lines_of "puzzle-input" in
  let bags = bag_successive_ints lines in
  BatEnum.map BatEnum.sum bags |> List.of_enum

let first_puzzle () =
  let max_sum = BatList.max summed_bags in
  Printf.printf "First puzzle, maximum bag of calories is %d\n" max_sum

let second_puzzle () = 
  let sorted_bags = List.sort (fun l r -> Int.compare r l) summed_bags in
  let top_three = List.take 3 sorted_bags in
  let sum_of_top_tree = List.sum top_three in
  Printf.printf "Second puzzle, sum of top three bags is %d\n" sum_of_top_tree

let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
