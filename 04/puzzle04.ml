open Batteries

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let string_split_map fn sep s =
  String.split ~by:sep s |> Tuple2.mapn fn

let assignment_of_string =
  string_split_map (string_split_map int_of_string "-") ","

let assignments =
  List.map assignment_of_string input


(* First part *)

let fully_contained (range_1, range_2) =
  let fully_contained' (big_start, big_end) (small_start, small_end) =
    big_start <= small_start && small_end <= big_end
  in
  fully_contained' range_1 range_2 || fully_contained' range_2 range_1

let first_puzzle () =
  Printf.printf "First puzzle, number of fully contained assignments: %d\n" (List.count_matching fully_contained assignments)


(* Second part *)

let ranges_overlap ((start_1, end_1), (start_2, end_2)) =
  start_1 <= end_2 && end_1 >= start_2
  
let second_puzzle () =
  Printf.printf "Second puzzle, number of overlapping assignments: %d\n" (List.count_matching ranges_overlap assignments)

  
let () =
  print_newline ();
  first_puzzle ();
  second_puzzle ()
