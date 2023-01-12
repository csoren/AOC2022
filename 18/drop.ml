open Batteries
open Extensions

module CoordSet = Set.Make(Coord)

let input =
  File.lines_of "puzzle-input" |> List.of_enum


let atoms =
  List.map Coord.of_string input  


let cluster =
   List.fold (Fun.flip CoordSet.add) CoordSet.empty atoms


let exists d p = 
  CoordSet.mem p d


let tuple3_add (a,b,c) (x,y,z) =
  (a+x, b+y, c+z)


let count_open_sides d p =
  let neighbours = [
    ( -1,  0,  0);
    (  1,  0,  0);
    (  0, -1,  0);
    (  0,  1,  0);
    (  0,  0, -1);
    (  0,  0,  1);
  ] in
  List.map (tuple3_add p) neighbours
  |> List.count_matching (fun p -> exists d p |> not)


let bounding_box d =
  let min_max fn = CoordSet.to_seq d |> Seq.map fn |> Seq.min_max in
  let (min_x, max_x) = min_max Tuple3.first in
  let (min_y, max_y) = min_max Tuple3.second in
  let (min_z, max_z) = min_max Tuple3.third in
  ((min_x, min_y, min_z), (max_x, max_y, max_z))
