open Batteries
open Extensions
open Model

let input =
  File.lines_of "puzzle-input" |> List.of_enum

let char_to_square = function
  | 'a'..'z' as ch ->
      { kind = Regular;
        height = (int_of_char ch) - (int_of_char 'a');
        visited = NotVisited
      }
  | 'S' ->
      { kind = Start;
        height = 0;
        visited = NotVisited
      }
  | 'E' ->
      { kind = End;
        height = (int_of_char 'z') - (int_of_char 'a');
        visited = Visited 0
      }
  | _ -> failwith "unexpected input"

let line_to_squares line =
  String.to_list line |> List.map char_to_square

let squares input =
  List.map line_to_squares input |> Matrix.of_rows

let find_first_kind kind =
  Matrix.findi_opt (fun _ _ v -> v.kind = kind) %> Option.get

let can_reach from_x from_y to_x to_y m =
  Matrix.coord_valid from_x from_y m
  && Matrix.coord_valid to_x to_y m
  && m.(to_x).(to_y).visited <> NotVisited
  && m.(to_x).(to_y).height - m.(from_x).(from_y).height <= 1

let reachable_neighbours x y m =
  [ (x - 1, y);
    (x + 1, y);
    (x, y - 1);
    (x, y + 1)
  ] |> List.filter (fun (to_x, to_y) -> can_reach x y to_x to_y m)

let find_best_neighbour m (x, y) =
  let undistance = function
    | (coord, Visited d) -> Some (coord, d)
    | _ -> None
  in
  reachable_neighbours x y m
  |> List.map (fun (x,y) -> ((x,y), m.(x).(y).visited))
  |> List.filter_map undistance
  |> List.map snd
  |> List.min_opt Int.compare

let iterate m =
  let r = Matrix.copy m in
  let set_visited ((x,y), d) = r.(x).(y) <- { m.(x).(y) with visited = Visited (d + 1)} in
  Matrix.coords m
  |> List.filter (fun (x,y) -> m.(x).(y).visited = NotVisited)
  |> List.map (fun (x,y) -> find_best_neighbour m (x,y) |> Option.map (fun d -> ((x,y), d)))
  |> List.iter (Option.may set_visited);
  r

let shortest_overall_path m =
  Matrix.coords m
  |> List.filter (fun (x,y) -> m.(x).(y).height = 0 && m.(x).(y).visited <> NotVisited)
  |> List.map (fun (x,y) -> distance_of m.(x).(y).visited)
  |> List.min ~cmp:Int.compare

let solve_part2 m =
  shortest_overall_path m

let rec solve_parts (start_x, start_y) m =
  match m.(start_x).(start_y).visited with
    | Visited d -> (d, solve_part2 m)
    | NotVisited -> solve_parts (start_x, start_y) (iterate m)

let solve input =
  let field = squares input in
  let (part1, part2) = solve_parts (find_first_kind Start field) field in
  Printf.printf "Part 1, distance %d\n" part1;
  Printf.printf "Part 2, distance %d\n" part2

let () =
  print_newline ();
  solve input
