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
  List.map (List.drop_while (not % Char.is_letter) % List.rev) lines

let get_next_number s =
  let start = String.drop_while (not % Char.is_digit) s in
  let num_str = String.take_while Char.is_digit start in
  let skipped = String.drop (String.length num_str) start in
  (String.to_int num_str, skipped)
  
let parse_instruction_line line =
  let (move, line') = get_next_number line in
  let (from, line'') = get_next_number line' in
  let (to', _) = get_next_number line'' in
  (move, from, to')
  
let parse_instructions = 
  List.map parse_instruction_line

let (state, instructions) =
  match List.group_at ~separator:String.is_empty input with
  | [state; instructions] -> (parse_state state, parse_instructions instructions)
  | _ -> failwith "invalid input"

let rec run_instruction state (count, from, dest) =
  if count = 0 then
    state
  else
    let from_stack = Array.get state (from - 1) in
    let dest_stack = Array.get state (dest - 1) in
    let head = List.hd from_stack in
    ignore @@ Array.set state (from - 1) (List.tl from_stack);
    ignore @@ Array.set state (dest - 1) (head :: dest_stack);
    run_instruction state (count - 1, from, dest)


let state' =
  List.fold_left run_instruction (Array.of_list state) instructions
  |> Array.to_list

let first_puzzle () =
  let heads = List.map List.hd state' |> String.of_list in
  Printf.printf "Part 1, top crates: %s\n" heads

let () =
  print_newline ();
  first_puzzle ()
