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
    | ("R", l) -> Horizontal (l)
    | ("L", l) -> Horizontal (-l) 
    | ("U", l) -> Vertical (-l) 
    | ("D", l) -> Vertical (l) 
    | _ -> failwith "parse error"

let decrement_move = function
  | Horizontal d -> Horizontal (d - Int.sign d)
  | Vertical d -> Vertical (d - Int.sign d)

let single_move_delta = function
  | Horizontal d -> (Int.sign d, 0)
  | Vertical d -> (0, Int.sign d)

let directions = List.map parse_line

let tail_move (dx, dy) =
  if Int.abs dx <= 1 && Int.abs dy <= 1 then (0, 0)
  else (Int.sign dx, Int.sign dy)

let step_position (hx, hy) = function
  | Horizontal x -> (hx + Int.sign x, hy    )
  | Vertical y   -> (hx             , hy + Int.sign y)

let step m head tail =
  let head' = step_position head m in
  let tail' = Tuple2.add tail (Tuple2.sub head' tail |> tail_move) in
  (head', tail')

let tail_positions input =
  let rec tail_positions'' positions head tail = function
  | Horizontal 0 | Vertical 0 -> (positions, head, tail)
  | m ->
      let (head', tail') = step m head tail in
      tail_positions'' (Set.add tail' positions) head' tail' (decrement_move m)
  in
  let rec tail_positions' positions head tail = function
    | [] -> positions
    | m :: rest ->
        let (positions', head', tail') = tail_positions'' positions head tail m in
        tail_positions' (Set.add_seq (positions' |> Set.to_seq) positions) head' tail' rest
    in
  tail_positions' (Set.singleton (0,0)) (0,0) (0,0) input

let solve_part1 input =
  tail_positions input |> Set.size  

let () =
  print_newline ();
  let d = directions input in
  Printf.printf "Part 1, positions visited: %d\n" (solve_part1 d);
