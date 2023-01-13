open Batteries
open Extensions

let neighbours c =
  List.map (Tuple3.add c)
  [ ( -1,  0,  0);
    (  1,  0,  0);
    (  0, -1,  0);
    (  0,  1,  0);
    (  0,  0, -1);
    (  0,  0,  1);
  ]


module Part1 = struct
  let count_open_sides d p =
    List.count_matching (fun p -> Drop.known d p |> not) (neighbours p)

  let solve () =
    List.map (count_open_sides Drop.initial_cluster) Drop.atoms |> List.sum
end


module Part2 = struct
  let fill cluster coord =
    if Drop.known cluster coord then begin
      (false, cluster)
    end else
      let neighbours = neighbours coord |> List.map (Drop.visit cluster) in
      let has_walls = List.exists (function Some Drop.Walls _ -> true | _ -> false) neighbours in
      if has_walls then
        let walls = List.count_matching ((=) (Some Drop.Atom)) neighbours in
        (true, Drop.add_atom cluster coord (Walls walls))
      else
        (false, cluster)


  let flood_fill cluster min max =
    let flood_fill' (filled, cluster) coord =
      let (filled', cluster') = fill cluster coord in
      (filled || filled', cluster')
    in
  Coord.range min max |> List.fold flood_fill' (false, cluster)


  let count_walls cluster min max =
    let rec fill cluster =
      let (filled, cluster') = flood_fill cluster min max in
      if filled then fill cluster'
      else (false, cluster')
    in
    let (_, cluster') = fill (Drop.add_atom cluster min (Drop.Walls 0)) in
    Drop.visits cluster' |> List.fold_left (fun s -> function | Drop.Walls n -> s + n | _ -> s) 0


  let bounding_box cluster =
    let (min, max) = Drop.bounding_box cluster in 
    (Tuple3.add min (-1,-1,-1), Tuple3.add max (1,1,1))

  let solve () =
    let (bound_min, bound_max) = bounding_box Drop.initial_cluster in
    count_walls Drop.initial_cluster bound_min bound_max
end


let () =
  print_newline ();
  Printf.printf "Part 1, surface area: %d\n" (Part1.solve ()); flush stdout;
  Printf.printf "Part 2, surface area: %d\n" (Part2.solve ())
