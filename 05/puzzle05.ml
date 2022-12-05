open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let parse_state_line =
  String.ntake 4 %> List.map (Fun.flip String.get 1)

let parse_state state =
  let stacks = List.map parse_state_line state |> List.rdrop 1 |> List.transpose in
  List.map (List.drop_while Char.is_whitespace) stacks

let parse_instruction_line line =
  let parts = String.split_on_char ' ' line |> Array.of_list in
  let move = Array.get parts 1 |> int_of_string in
  let from = Array.get parts 3 |> int_of_string in
  let dest = Array.get parts 5 |> int_of_string in
  (move, from - 1, dest - 1)
  
let parse_instructions = 
  List.map parse_instruction_line

let (state, instructions) =
  List.split_match String.is_empty input 
  |> Tuple2.map parse_state parse_instructions

let execute_instruction fn state (count, from, dest) =
  let top = Array.get state from |> List.take count |> fn in
  Array.get_set (List.append top) state dest;
  Array.get_set (List.drop count) state from
  
let solve execute =
  let state' = Array.of_list state in
  List.iter (execute state') instructions;
  state' |> Array.to_list |> List.heads |> String.of_list

let print_part part fn =
  Printf.printf "Part %d, top crates: %s\n" part (solve @@ execute_instruction fn)


let () =
  print_newline ();
  print_part 1 List.rev;
  print_part 2 Fun.id
