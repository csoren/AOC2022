open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

type instruction =
  | Noop
  | Addx of int

let line_to_instruction line =
  match String.split_on_char_greedy ' ' line with
    | "noop" :: _ -> Noop
    | "addx" :: v :: _ -> Addx (String.to_int v)
    | _ -> failwith "unexpected input"

let register_contents instructions =
  let execute reg = function
    | Noop -> [ reg ]
    | Addx v -> [ reg; reg + v ]
  in
  let cycles = List.fold (fun regs instruction -> execute (List.last (List.hd regs)) instruction :: regs) [ [1; 1] ] instructions |> List.rev |> List.flatten in
  cycles

let interesting_cycles =
  [ 20; 60; 100; 140; 180; 220 ]

let solve_part1 input =
  let registers = register_contents input in
  (* print_endline (List.int_list_to_string registers); *)
  let strengths = interesting_cycles |> List.map (fun cycle -> List.at registers cycle * cycle) in
  List.sum strengths

let () =
  let instructions = input |> List.map line_to_instruction in
  print_newline ();
  Printf.printf "Part 1: signal strength: %d\n" (solve_part1 instructions)
