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
  List.fold (fun regs instruction -> execute (List.last (List.hd regs)) instruction :: regs) [ [1; 1] ] instructions
  |> List.rev |> List.flatten

let solve_part1 registers =
  let interesting_cycles = [ 20; 60; 100; 140; 180; 220 ] in
  let strengths = interesting_cycles |> List.map (fun cycle -> List.at registers cycle * cycle) in
  List.sum strengths
  
let x_register_to_crt_line xs =
  List.mapi (fun index x -> if index >= x - 1 && index <= x + 1 then '#' else '.') xs |> String.implode

let solve_part2 registers =
  let lines_x_values = List.ntake 40 (List.drop 1 registers) in
  let lines = lines_x_values |> List.map x_register_to_crt_line in
  List.iter print_endline lines

let () =
  let instructions = input |> List.map line_to_instruction in
  let registers = register_contents instructions in
  print_newline ();
  Printf.printf "Part 1: signal strength: %d\n" (solve_part1 registers);
  solve_part2 registers
