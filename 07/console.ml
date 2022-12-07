open Batteries
open Extensions

type command =
| Cd of string
| Ls

type process = command * string list

let input =
  File.lines_of "test-input" |> List.of_enum

let to_command line =
  match String.split_on_char ' ' line with
  | "cd" :: args -> Cd (List.hd args)
  | "ls" :: _ -> Ls
  | _ -> failwith "Unknown command"

let to_process = function
  | input :: output ->
      let cmd = to_command (String.sub input 2 @@ String.length input - 2) in
      (cmd, output)
  | _ -> failwith "Empty process"

let processes lines =
  List.group_when ~first:(String.starts_with "$") lines
  |> List.map to_process
