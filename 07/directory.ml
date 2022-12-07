open Batteries

type entry =
  | Directory of directory
  | File of int
and directory =
  (string * entry) list

let to_string d =
  let rec entry_to_string indent = function
    | (name, File size) -> Printf.sprintf "%s%d %s" indent size name
    | (name, Directory dir) -> Printf.sprintf "%s[%s]\n%s" indent name (list_to_string (indent ^ "  ") dir)
  and list_to_string indent l =
    List.map (entry_to_string indent) l |> String.join "\n"
  in
  list_to_string "" d

let root =
  [ ("/", Directory []) ]

let find (dir: directory) name =
  match List.find_opt (fun (n, _) -> n = name) dir with
  | Some (_, f) -> f
  | _ -> failwith (Printf.sprintf "File %s not found in directory" name)

let find_directory dir name =
  match find dir name with
  | Directory files -> files
  | _ -> failwith "directory does not exist"

let remove (dir: directory) name: directory =
  List.filter (fun (n, _) -> n <> name) dir
  
let replace dir name entry: directory =
  remove dir name |> List.cons (name, entry)

let append = List.append
