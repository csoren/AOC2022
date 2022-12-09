open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let char_to_digit ch =
  (int_of_char ch) - (int_of_char '0')

let forest lines =
  lines |> List.map (String.to_list %> List.map char_to_digit) |> Matrix.of_rows

let all_less_than h l =
  match l with
    | [ ] -> true
    | l -> List.for_all (fun v -> h > v) l
  
let get_right m x y = Matrix.sub_row_right_edge m (x + 1) y

let get_left m x y = Matrix.sub_row_left_edge m (x - 1) y

let get_down m x y = Matrix.sub_column_bottom_edge m x (y + 1)

let get_up m x y = Matrix.sub_column_top_edge m x (y - 1)
  
let is_visible m x y =
  let h = m.(x).(y) in
  let is_visible' l = List.exists (fun f -> f m x y |> all_less_than h) l in
  [get_left; get_right; get_up; get_down]
  |> is_visible'

let count_visible_in_row m y =
  List.range 1 `To (Matrix.width m - 2)
  |> List.count_matching (fun x ->
    is_visible m x y |> tap (Printf.printf "%d,%d=%b\n" x y))

let count_visible m =
  List.range 1 `To (Matrix.height m - 2)
  |> List.map (fun y -> count_visible_in_row m y)
  |> List.sum

let solve lines =
  let m = forest lines in
  count_visible m + (Matrix.width m * 2 + 2 * Matrix.height m - 4)


let () =
  print_newline ();
  Printf.printf "Part 1, visible trees: %d\n" (solve input)
