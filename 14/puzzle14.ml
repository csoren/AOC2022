open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let initial_sand_pos = (500, 0)
  
let print_rock_paths p =
  let s = List.to_string RockPath.to_string p in
  print_endline s

let cell_to_char = function
  | Field.Empty -> '.'
  | Field.Rock -> '#'
  | Field.Sand -> '+'

let row_to_string =
  List.map cell_to_char %> String.of_list

let print_field field =
  Field.rows field |> List.map row_to_string |> List.iter print_endline

type move = | MovedTo of (int * int) | Settled | Dropped

let move_sand field (x, y) =
  let candidates = [(x, y+1); (x-1, y+1); (x+1, y+1)] in
  match List.find_opt (fun (x, y) -> Field.get field x y = Field.Empty) candidates with
    | None -> Field.set field x y Sand; Settled
    | Some (x, y) -> if Field.inside field x y then MovedTo (x, y) else Dropped

let drop_sand field =
  let rec drop_sand' pos = 
    match move_sand field pos with
    | MovedTo v -> drop_sand' v
    | Settled -> false
    | Dropped -> true
  in
  drop_sand' initial_sand_pos

let count_until_dropped field =
  let rec count n =
    if drop_sand field then n
    else count (n + 1)
  in
  count 0

let solve_part1 field =
  count_until_dropped field


let () =
  print_newline ();
  let rock_paths = List.map RockPath.of_string input in
  let field = Field.of_paths rock_paths initial_sand_pos in
  print_field field;
  Printf.printf "Part 1: %d\n" (solve_part1 field)
