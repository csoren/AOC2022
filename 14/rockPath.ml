open Extensions

type rock_path = (int * int) list

let coord_to_string (x, y) =
  Printf.sprintf "(%i,%i)" x y

let to_string p =
  List.to_string coord_to_string p

let string_to_coord s =
  let (x, y) = String.split ~by:"," s in
  (String.to_int x, String.to_int y)
  
let of_string s =
  String.split_on_string ~by:" -> " s |> List.map string_to_coord
