open Batteries
open Extensions


(* Puzzle input and parsing *)

let test_input =
  (10, 20, File.lines_of "test-input" |> List.of_enum)

let puzzle_input =
  (2000000, 4000000, File.lines_of "puzzle-input" |> List.of_enum)

let string_to_coord s =
  let (x, y) = String.split ~by:"," s in
  let extract_int s prefix = 
    let start = String.find s prefix + (String.length prefix) in
    String.sub s start (String.length s - start)
  in
  (extract_int x "x=" |> String.to_int, extract_int y "y=" |> String.to_int)

let line_to_coord_pair line =
  let (sensor, beacon) = String.split ~by:":" line in
  (string_to_coord sensor, string_to_coord beacon)

let sensors =
  List.map line_to_coord_pair


(* Utilities *)  
let tuple2_to_string =
  Tuple2.to_string Int.to_string Int.to_string
    
let manhattan_distance (sx, sy) (bx, by) =
  (Int.abs (bx - sx)) + (Int.abs (by - sy))
  

(* Puzzle solving *)
let sensors_distances sensors =
  List.map (fun (sensor, beacon) -> (sensor, manhattan_distance sensor beacon)) sensors

let range_for_sensor y ((sx, sy), d) =
  let width = d - (Int.abs @@ sy - y) in
  if width > 0 then Some (sx - width, sx + width)
  else None

let beacons_on_row sensors y =
  List.map snd sensors |> List.filter (snd %> (=) y) |> List.map fst |> List.unique ~eq:(=)

let ranges_for_row sensors y =
  List.flat_map_opt (range_for_sensor y) sensors |> Range.of_list

let solve_part1 (y, _, input) =
  let sensors = sensors input in
  let sensors' = List.map (fun (s, b) -> (s, manhattan_distance s b)) sensors in
  let ranges = ranges_for_row sensors' y in
  let beacons_contained = List.count_matching (Range.cover ranges) (beacons_on_row sensors y) in
  (List.map (fun (start,stop) -> stop - start + 1) ranges |> List.sum) - beacons_contained

let solve_part2 (_, rows, input) =
  let sensors = sensors_distances @@ sensors input in
  let (x, y) =
    List.range 0 `To rows |> List.find_map (fun y ->
      if Int.modulo y 500000 = 0 then Printf.printf "%d\n" y; flush stdout; 
      match ranges_for_row sensors y with
      | r1 :: r2 :: [] -> Some (snd r1 + 1, y)
      | _ -> None)
  in
  Printf.printf "Location %d,%d\n" x y;
  x * 4000000 + y

let () =
  print_newline ();
  Printf.printf "Part 1: %d\n" (solve_part1 puzzle_input);
  Printf.printf "Part 2: %d\n" (solve_part2 puzzle_input)
