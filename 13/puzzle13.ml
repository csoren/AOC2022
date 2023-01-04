open Batteries
open Extensions

let input =
  match RecursiveList.of_file "puzzle-input" with
  | Some v -> v
  | None -> failwith "parsing failed" 
  
let orders l =
  List.map (fun (p1, p2) -> RecursiveList.compare p1 p2) l

let solve_part1 l =
  orders l 
  |> List.filteri_map (fun i v -> if v < 0 then Some (i + 1) else None)
  |> List.sum

let () =
  print_newline ();
  Printf.printf "Sum of indices: %d\n" (solve_part1 input)
