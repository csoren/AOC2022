open Batteries
open Extensions
open Model

let input =
  File.lines_of "test-input" |> List.of_enum

let char_to_square = function
  | 'a'..'z' as ch ->
      { kind = Regular;
        height = (int_of_char ch) - (int_of_char 'a');
        blocked = false
      }
  | 'S' ->
      { kind = Start;
        height = 0;
        blocked = false
      }
  | 'E' ->
      { kind = End;
        height = (int_of_char 'z') - (int_of_char 'a');
        blocked = false
      }
  | _ -> failwith "unexpected input"

let line_to_squares line =
  String.to_list line |> List.map char_to_square

let squares input =
  List.map line_to_squares input |> Matrix.of_rows

let position_of kind =
  Matrix.findi_opt (fun _ _ v -> v.kind = kind) %> Option.get

let can_reach from_x from_y to_x to_y m =
  Matrix.coord_valid from_x from_y m
  && Matrix.coord_valid to_x to_y m
  && not m.(to_x).(to_y).blocked
  && m.(to_x).(to_y).height - m.(from_x).(from_y).height <= 1

let goals x y m =
  [ (x - 1, y);
    (x + 1, y);
    (x, y - 1);
    (x, y + 1)
  ] |> List.filter (fun (to_x, to_y) -> can_reach x y to_x to_y m)

let rec distance_to_goal x y m =
  if m.(x).(y).kind = End then
    Some 0
  else begin
    m.(x).(y) <- { m.(x).(y) with blocked = true };
    let distances =
      goals x y m
      |> List.map (fun (x,y) -> distance_to_goal x y m)
      |> List.filter Option.is_some
    in
    let r = match distances with
      | [] -> None
      | l -> List.map Option.get l |> List.min |> (fun v -> Some (v + 1))
    in
    m.(x).(y) <- { m.(x).(y) with blocked = false };
    r
  end

let solve_part1 input =
  let field = squares input in
  let (start_x, start_y) = position_of Start field in
  let distance = distance_to_goal start_x start_y field in
  Printf.printf "Distance %d\n" (Option.get distance)

let () =
  print_newline ();
  solve_part1 input
