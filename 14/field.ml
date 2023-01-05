open Extensions


type cell = | Empty | Rock | Sand

type t = {
  x: int;
  y: int;
  field: cell Matrix.t;
}


let height field = Matrix.height field.field

let get_row field row =
  Matrix.row (row - field.y) field.field

let rows field =
  List.range 0 `To (height field - 1) |> List.map (Fun.flip Matrix.row field.field)

let set field x y v =
  Matrix.set field.field (x - field.x) (y - field.y) v


let rec add_vertical_line field x from_y to_y =
  if 
    from_y > to_y then add_vertical_line field x to_y from_y
  else
    List.range from_y `To to_y |> List.iter (fun y -> set field x y Rock)


let rec add_horizontal_line field y from_x to_x =
  if 
    from_x > to_x then add_horizontal_line field y to_x from_x
  else
    List.range from_x `To to_x |> List.iter (fun x -> set field x y Rock)


let add_line field (from_x, from_y) (to_x, to_y) =
  if from_x = to_x then
    add_vertical_line field from_x from_y to_y
  else
    add_horizontal_line field from_y from_x to_x


let of_paths paths start =
  let lines = start :: List.flatten paths in
  let (min_x, max_x) = List.map fst lines |> List.min_max ~cmp:Int.compare in
  let (min_y, max_y) = List.map snd lines |> List.min_max ~cmp:Int.compare in
  let field = {
    x = min_x;
    y = min_y;
    field = Matrix.make (max_x - min_x + 1) (max_y - min_y + 1) Empty
  } in
  let add_path path = List.window_tuplets path |> List.iter (fun (f,t) -> add_line field f t) in
  List.iter add_path paths;
  field
