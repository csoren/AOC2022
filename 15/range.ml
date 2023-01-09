open Extensions

(* Functions for manipulating list of ranges *)

let no_overlap r h =
  snd r < fst h || fst r > snd h 

let overlaps_right candidate r =
  (fst candidate) <= (snd r) && (snd candidate) > (snd r)

let overlaps_left candidate r =
  (fst candidate < fst r) && (snd candidate) >= (fst r)

let rec split_range r rs =
  match rs with
  | h :: tail ->
      if no_overlap r h then
        split_range r tail
      else if overlaps_left r h then
        (fst r, fst h - 1) :: split_range (fst h, snd r) rs
      else if overlaps_right r h then
        split_range (snd h + 1, snd r) rs
      else
        []
  | [] -> [ r ] 

let sort_range =
  List.sort (fun t1 t2 -> Int.compare (fst t1) (fst t2))

let rec add rs r =
  let rs = sort_range rs in
  split_range r rs @ rs

let rec merge = function
  | h1 :: h2 :: tail when snd h1 + 1 = fst h2 ->
      merge ((fst h1, snd h2) :: tail)
  | h1 :: tail ->
      h1 :: (merge tail)
  | [] -> []

let cover rs v =
  List.exists (fun (s, e) -> v >= s && v <= e) rs
  
let of_list rs =
  List.fold add [] rs |> merge
  