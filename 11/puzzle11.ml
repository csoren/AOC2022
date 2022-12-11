open Batteries
open Extensions
open Opal

let input =
  LazyStream.of_channel (In_channel.open_bin "test-input")



let () =
  match Parser.parse input with
  | None -> failwith "Parsing failed"
  | Some monkeys -> List.iter (Model.monkey_to_string %> print_endline) monkeys
