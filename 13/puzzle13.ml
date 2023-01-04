open Batteries
open Extensions

let input =
  match RecursiveList.of_file "puzzle-input" with
    | Some v -> v
    | None -> failwith "parsing failed" 

let dividers =
  match RecursiveList.of_string "[[2]]\n[[6]]\n" with
    | Some v -> v
    | None -> failwith "parsing failed"

let (divider1, divider2) = 
  List.hd dividers
    
let orders l =
  List.map (fun (p1, p2) -> RecursiveList.compare p1 p2) l

let solve_part1 l =
  orders l 
  |> List.filteri_map (fun i v -> if v < 0 then Some (i + 1) else None)
  |> List.sum

let solve_part2 l =
  let sorted = List.flat_map (fun (l, r) -> [l; r]) l |> List.sort RecursiveList.compare in
  let find d = List.findi (fun i v -> RecursiveList.compare v d = 0) sorted |> fst |> (+) 1 in
  let (index1, index2) = (find divider1, find divider2) in
  index1 * index2

let () =
  print_newline ();
  Printf.printf "Part 1: %d\n" (solve_part1 input);
  Printf.printf "Part 2: %d\n" (solve_part2 (dividers @ input))
