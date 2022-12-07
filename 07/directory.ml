open Batteries

type entry =
  | Directory of directory
  | File of int
and directory =
  (string * entry) list

let find (dir: directory) name =
  List.find (fun (n, _) -> n = name) dir |> snd

let find_directory dir name =
  match find dir name with
  | Directory files -> files
  | _ -> failwith "directory does not exist"

let remove (dir: directory) name: directory =
  List.filter (fun (n, _) -> n <> name) dir
  
let replace dir name entry: directory =
  remove dir name |> List.cons (name, entry)
