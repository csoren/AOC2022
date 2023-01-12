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
