open Batteries
open Extensions

type t = int * int * int


let compare (a, b, c) (x, y, z) =
  match Int.compare a x with
  | 0 -> begin
      match Int.compare b y with
      | 0 -> Int.compare c z
      | r -> r
    end
  | r -> r


let of_string s =
  match (String.split_on_char_greedy ',' s |> List.map String.to_int) with
  | x :: y :: z :: [] -> (x, y, z)
  | _ -> failwith "parse error"
