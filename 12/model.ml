open Extensions

type square_kind =
  | Start
  | End
  | Regular

type square_status =
  | NotVisited
  | Visited of int

type square = {
  kind: square_kind;
  height: int;
  mutable visited: square_status;
}

let square_to_string s =
  match s.visited with
  | NotVisited -> Printf.sprintf "[%02d  .]" s.height
  | Visited distance -> Printf.sprintf "[%02d %02d]" s.height distance

let field_row_to_string row =
  List.map square_to_string row |> String.join " "

let field_to_string m =
  Matrix.rows m |> List.map field_row_to_string |> String.join "\n"

