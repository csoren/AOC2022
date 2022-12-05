open Batteries

include List

let to_string conv list =
  let elements = map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let string_list_to_string = to_string (fun s -> "\"" ^ s ^ "\"")

let int_list_to_string = to_string string_of_int

let int_list_list_to_string = to_string int_list_to_string

let group_at ~separator:sep l =
  List.nsplit sep l |> List.filter (not % List.is_empty)

let split_match p list =
  List.fold_while (fun _ el -> not @@ p el) (fun acc el -> el :: acc) [] list
  |> fun (first, second) -> (List.rev first, List.tl second)

let heads l = List.map List.hd l

let tails l = List.map List.tl l

let rec transpose = function
  | [] :: _ -> []
  | l -> heads l :: (tails l |> transpose)

let rdrop n = List.rev %> List.drop n %> List.rev 