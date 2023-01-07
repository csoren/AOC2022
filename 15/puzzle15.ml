open Batteries
open Extensions

let test_input =
  (10, File.lines_of "test-input" |> List.of_enum)

let puzzle_input =
  (2000000, File.lines_of "puzzle-input" |> List.of_enum)

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

let distance (sx, sy) (bx, by) =
  (Int.abs (bx - sx)) + (Int.abs (by - sy))

let sensors =
  List.map line_to_coord_pair

let sensors_distances sensors =
  List.map (fun (sensor, beacon) -> (sensor, distance sensor beacon)) sensors

let coord_cannot_be_beacon sensors x y = 
  List.exists (fun (sensor, d) -> distance sensor (x, y) <= d) sensors

let solve_part1 (y, input) =
  let sensors = sensors input in
  let beacons = List.map snd sensors |> List.unique ~eq:(Tuple2.eq (=) (=)) in
  let coords = List.flat_map (fun (c1, c2) -> [c1; c2]) sensors in
  let (min_x, max_x) = List.map fst coords |> List.min_max ~cmp:Int.compare in
  let (min_y, max_y) = List.map snd coords |> List.min_max ~cmp:Int.compare in
  let sensors = List.map (fun (s, b) -> (s, distance s b)) sensors in
  Printf.printf "Board size: x %d;%d, y %d;%d\n" min_x max_x min_y max_y;
  (* let line y = List.range min_x `To max_x |> List.map (fun x -> if coord_cannot_be_beacon sensors beacons x y then '#' else '.') |> String.of_list in
  print_endline (line y); *)
  (* List.range min_y `To max_y |> List.map line |> List.iter print_endline; *)
  (List.range (-1100000) `To 4400000 |> List.count_matching (fun x -> coord_cannot_be_beacon sensors x y)) - (List.count_matching (fun (_,by) -> y = by) beacons)

let () =
  print_newline ();
  Printf.printf "Part 1: %d\n" (solve_part1 puzzle_input)
