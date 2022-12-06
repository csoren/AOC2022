open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum |> List.hd |> String.to_list

let find_distinct n =
  let match_tuple _ l = (Set.of_list l |> Set.size) = n in
  List.window n input |> List.findi match_tuple |> fst

let print_part1 () =
  Printf.printf "Part 1, index = %d\n" (find_distinct 4)

let print_part2 () =
  Printf.printf "Part 2, index = %d\n" (find_distinct 14)


let () =
  print_newline ();
  print_part1 ();
  print_part2 ()
