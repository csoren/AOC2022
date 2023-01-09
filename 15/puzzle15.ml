open Batteries
open Extensions

let test_input =
  (10, 20, File.lines_of "test-input" |> List.of_enum)

let puzzle_input =
  (2000000, 4000000, File.lines_of "puzzle-input" |> List.of_enum)

let tuple2_to_string =
  Tuple2.to_string Int.to_string Int.to_string
  
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

let no_overlap r h =
  snd r < fst h || fst r > snd h 

let overlaps_right candidate r =
  (fst candidate) <= (snd r) && (snd candidate) > (snd r)

let overlaps_left candidate r =
  (fst candidate < fst r) && (snd candidate) >= (fst r)

let rec split_range r rs =
  match rs with
  | h :: tail ->
      if no_overlap r h then
        split_range r tail
      else if overlaps_left r h then
        (fst r, fst h - 1) :: split_range (fst h, snd r) rs
      else if overlaps_right r h then
        split_range (snd h + 1, snd r) rs
      else
        []
  | [] -> [ r ] 

let sort_range =
  List.sort (fun t1 t2 -> Int.compare (fst t1) (fst t2))

let rec add_range rs r =
  let rs = sort_range rs in
  split_range r rs @ rs

let range_for_sensor y ((sx, sy), d) =
  let width = d - (Int.abs @@ sy - y) in
  if width > 0 then Some (sx - width, sx + width)
  else None

let ranges_contain l v =
  List.exists (fun (s, e) -> v >= s && v <= e) l

let beacons_on_row sensors y =
  List.map snd sensors |> List.filter (snd %> (=) y) |> List.map fst |> List.unique ~eq:(=)

let rec merge_ranges = function
  | h1 :: h2 :: tail when snd h1 + 1 = fst h2 ->
      merge_ranges ((fst h1, snd h2) :: tail)
  | h1 :: tail ->
      h1 :: (merge_ranges tail)
  | [] -> []

let ranges_for_row sensors y =
  List.flat_map_opt (range_for_sensor y) sensors |> List.fold add_range [] |> merge_ranges

let solve_part1 (y, _, input) =
  let sensors = sensors input in
  let sensors' = List.map (fun (s, b) -> (s, distance s b)) sensors in
  let ranges = List.flat_map_opt (range_for_sensor y) sensors' |> List.fold add_range [] in
  let beacons_contained = List.count_matching (ranges_contain ranges) (beacons_on_row sensors y) in
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
