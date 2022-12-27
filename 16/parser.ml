open Batteries
open Extensions
open Model
open Opal

let debug s = print_endline s; return ()

let digits = spaces >> many1 digit => String.implode => int_of_string

let valve_name =
  spaces >>
  upper >>= fun first ->
  upper >>= fun second ->
  return (Printf.sprintf "%c%c" first second)

let valve_list = sep_by1 valve_name (token ", ")

let plural s = spaces >> token s >> optional (token "s")

let room =
  token "Valve" >> spaces >> 
  lexeme valve_name >>= fun room ->
  lexeme @@ token "has flow rate=" >> digits << token ";" >>= fun flow_rate ->
  plural "tunnel" >> plural "lead" >> token "to" >> plural "valve" >> valve_list >>= fun valves ->
  return  {
    valve = room;
    pressure = flow_rate;
    connection_names = valves;
    connections = [];
    blind = false
  }    

let room_list =
  many1 room

let parse = Opal.parse room_list
