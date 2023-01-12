open Batteries
open Extensions

type t = int * int * int


let compare = Stdlib.compare


let of_string s =
  match (String.split_on_char_greedy ',' s |> List.map String.to_int) with
  | x :: y :: z :: [] -> (x, y, z)
  | _ -> failwith "parse error"


let to_string (x, y, z) =
  Printf.sprintf "(%d,%d,%d)" x y z
  