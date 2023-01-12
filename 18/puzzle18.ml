open Batteries
open Extensions

let solve_part1 () =
  List.map (Drop.count_open_sides Drop.cluster) Drop.atoms |> List.sum



let () =
  print_newline ();
  let (box_min, box_max) = Drop.bounding_box Drop.cluster in
  Printf.printf "%s to %s\n" (Coord.to_string box_min) (Coord.to_string box_max);
  Printf.printf "Part 1, surface area: %d\n" (solve_part1 ())
