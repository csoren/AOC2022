open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let char_to_digit ch =
  (int_of_char ch) - (int_of_char '0')

let forest lines =
  lines |> List.map (String.to_list %> List.map char_to_digit) |> Matrix.of_rows

let get_right m x y = Matrix.sub_row_right_edge m (x + 1) y

let get_left m x y = Matrix.sub_row_left_edge m (x - 1) y

let get_down m x y = Matrix.sub_column_bottom_edge m x (y + 1)

let get_up m x y = Matrix.sub_column_top_edge m x (y - 1)
  
module Part1 = struct
  let all_less_than h = function
    | [ ] -> true
    | l -> List.for_all ((>) h) l
    
  let is_visible m x y =
    let h = m.(x).(y) in
    [get_left; get_right; get_up; get_down]
    |> List.exists (fun f -> f m x y |> all_less_than h)
  
  let count_visible m =
    Matrix.sub_coords m (1, Matrix.width m - 2) (1, Matrix.height m - 2)
    |> List.count_matching (fun (x,y) -> is_visible m x y)

  let solve m =
    count_visible m + (Matrix.width m * 2 + 2 * Matrix.height m - 4)

end


module Part2 = struct
  let viewing_distance x y h m f =
    let trees = f m x y in
    List.findi_opt ((<=) h) trees
    |> Option.map_default (fst %> (+) 1) (List.length trees)

  let scenic_score m (x, y) =
    let h = m.(x).(y) in
    [get_left; get_right; get_up; get_down]
    |> List.map (viewing_distance x y h m)
    |> List.fold ( * ) 1

  let solve m =
    Matrix.sub_coords m (1, Matrix.width m - 2) (1, Matrix.height m - 2)
    |> List.map (scenic_score m) 
    |> List.max ~cmp:Int.compare

end


let () =
  print_newline ();
  let m = forest input in
  Printf.printf "Part 1, visible trees: %d\n" (Part1.solve m);
  Printf.printf "Part 2, best scenic score: %d\n" (Part2.solve m)
