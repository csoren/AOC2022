open Batteries
open Extensions

let input =
  File.lines_of "test-input" |> List.of_enum
  
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

let () =
  print_newline ();
  let rock_paths = List.map RockPath.of_string input in
  let field = Field.of_paths rock_paths (500, 0) in
  print_field field