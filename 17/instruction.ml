open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum |> List.hd

type instruction = Left | Right

let char_to_instruction = function
  | '<' -> Left
  | '>' -> Right
  | _ -> failwith "this never happens"

let all =
  input |> String.explode |> List.map char_to_instruction

let cycle =
  Seq.cycle (Seq.of_list all)