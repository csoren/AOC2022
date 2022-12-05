open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let heads = List.map List.hd

let tails = List.map List.tl

let rec transpose = function
  | [] :: _ -> []
  | l -> heads l :: (tails l |> transpose)

let parse_state state =
  let read_state_line =
    String.explode %> List.ntake 4 %> List.map (List.drop 1 %> List.hd) in
  let lines =
    List.map read_state_line state |> List.rev |> List.drop 1 |> transpose in
  List.map (List.drop_while Char.is_whitespace %> List.rev) lines

let get_next_number s =
  let start = String.drop_while (not % Char.is_digit) s in
  let num_str = String.take_while Char.is_digit start in
  let skipped = String.drop (String.length num_str) start in
  (String.to_int num_str, skipped)
  
let parse_instruction_line line =
  let (move, line') = get_next_number line in
  let (from, line'') = get_next_number line' in
  let (to', _) = get_next_number line'' in
  let _ = Printf.printf "%d %d %d\n" move from to' in
  (move, from, to')
  
let parse_instructions = List.map parse_instruction_line

let (state, _) =
  match List.group_at ~separator:String.is_empty input with
  | [state; instructions] -> (parse_state state, parse_instructions instructions)
  | _ -> failwith "invalid input"

let first_puzzle () =
  print_endline (List.to_string String.of_list state);
  ()

let () =
  print_newline ();
  first_puzzle ()
