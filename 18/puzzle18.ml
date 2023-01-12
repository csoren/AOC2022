open Batteries
open Extensions

let solve_part1 () =
  List.map (Drop.count_open_sides Drop.cluster) Drop.atoms |> List.sum

let () =
  print_newline ();
  Printf.printf "Part 1, surface area: %d\n" (solve_part1 ())
