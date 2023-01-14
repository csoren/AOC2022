open Batteries
open Extensions
open Opal

let input =
  LazyStream.of_channel (In_channel.open_bin "puzzle-input")

let blueprints =
  match Parser.parse input with
  | Some b -> b
  | None -> failwith "parse error"

let () =
  print_newline ()
