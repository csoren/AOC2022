open Batteries
open Extensions

type command =
| Cd of string
| Ls

type process = command * string list

let command_to_string = function
  | Cd dir -> "cd " ^ dir
  | Ls -> "ls"

let to_command line =
  match String.split_on_char ' ' line with
  | "cd" :: args -> Cd (List.hd args)
  | "ls" :: _ -> Ls
  | _ -> failwith "Unknown command"

let to_process = function
  | input :: output ->
      let cmd = to_command (String.drop 2 input) in
      (cmd, output)
  | _ -> failwith "Empty process"

let processes lines =
  List.group_when ~first:(Fun.flip String.starts_with "$ ") lines
  |> List.map to_process
