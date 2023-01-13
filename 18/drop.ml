open Batteries
open Extensions

type visit =
  Atom | Walls of int

module CoordMap = Map.Make(Coord)

let input =
  File.lines_of "puzzle-input" |> List.of_enum


let atoms =
  List.map Coord.of_string input  


let add_atoms cluster atoms =
  List.fold (fun m c -> CoordMap.add c Atom m) cluster atoms


let initial_cluster =
   add_atoms CoordMap.empty atoms


let known d p =
  CoordMap.mem p d


let visit d p =
  CoordMap.find_opt p d


let bounding_box d =
  let min_max fn = CoordMap.to_seq d |> Seq.map (fst %> fn) |> Seq.min_max in
  let (min_x, max_x) = min_max Tuple3.first in
  let (min_y, max_y) = min_max Tuple3.second in
  let (min_z, max_z) = min_max Tuple3.third in
  ((min_x, min_y, min_z), (max_x, max_y, max_z))


let add_atom cluster coord v =
  CoordMap.add coord v cluster  


let visits cluster =
  CoordMap.to_seq cluster |> Seq.map snd |> List.of_seq
