open Batteries
open Extensions

let input =
  File.lines_of "test-input" |> List.of_enum

let char_to_digit ch =
  (int_of_char ch) - (int_of_char '0')

let forest lines =
  lines |> List.map (String.to_list %> List.map char_to_digit) |> Matrix.of_rows

let all_less_than h = List.for_all (fun v -> h > v)
  
let get_right m x y = Matrix.sub_row_right_edge m (x + 1) y

let get_left m x y = Matrix.sub_row_left_edge m (x - 1) y

let get_down m x y = Matrix.sub_column_bottom_edge m x (y + 1)

let get_up m x y = Matrix.sub_column_top_edge m x (y - 1)
  
let is_visible m x y =
  let h = m.(x).(y) in
  [get_left; get_right; get_up; get_down]
  |> List.exists (fun f -> f m x y |> all_less_than h) 

let count_visible_in_row m y =
  List.range 0 `To (Matrix.width m - 1)
  |> List.count_matching (fun x -> is_visible m x y)

let count_visible m =
  List.range 0 `To (Matrix.height m - 1)
  |> List.map (fun y -> count_visible_in_row m y)
  |> List.sum

let solve lines =
  forest lines |> count_visible 

let () =
  print_newline ();
  Printf.printf "Part 1, visible trees: %d\n" (solve input)
  