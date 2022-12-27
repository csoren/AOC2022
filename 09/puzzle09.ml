open Batteries
open Extensions

type direction =
  | Horizontal of int
  | Vertical of int

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let parse_line line =
  let (d, l) = String.split ~by:" " line in
  match (d, Int.of_string l) with
    | ("R", l) -> List.range 1 `To l |> List.map (fun _ -> ( 1,  0))
    | ("L", l) -> List.range 1 `To l |> List.map (fun _ -> (-1,  0))
    | ("U", l) -> List.range 1 `To l |> List.map (fun _ -> ( 0, -1))
    | ("D", l) -> List.range 1 `To l |> List.map (fun _ -> ( 0,  1))
    | _ -> failwith "parse error"

let directions = List.flat_map parse_line

let tail_move (dx, dy) =
  if Int.abs dx <= 1 && Int.abs dy <= 1 then (0, 0)
  else (Int.sign dx, Int.sign dy)

let step m head tail =
  let head' = Tuple2.add head m in
  let tail' = Tuple2.add tail (Tuple2.sub head' tail |> tail_move) in
  (head', tail')

let tail_positions input =
  let rec tail_positions' positions head tail = function
    | [] -> positions
    | m :: rest ->
        let (head', tail') = step m head tail in
        tail_positions' (Set.add tail' positions) head' tail' rest
    in
  tail_positions' (Set.singleton (0,0)) (0,0) (0,0) input

let solve_part1 input =
  tail_positions input |> Set.size  

let () =
  print_newline ();
  let d = directions input in
  Printf.printf "Part 1, positions visited: %d\n" (solve_part1 d);
