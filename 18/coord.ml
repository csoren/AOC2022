open Batteries
open Extensions

type t = int * int * int


let compare = Stdlib.compare


let of_string s =
  match (String.split_on_char_greedy ',' s |> List.map String.to_int) with
  | x :: y :: z :: [] -> (x, y, z)
  | _ -> failwith "parse error"


let range (x1, y1, z1) (x2, y2, z2) =
  List.range x1 `To x2 |> List.flat_map (fun x -> List.range y1 `To y2 |> List.flat_map (fun y -> List.range z1 `To z2 |> List.map (fun z -> x, y, z)))
