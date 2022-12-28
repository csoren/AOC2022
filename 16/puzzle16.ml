open Batteries
open Extensions
open Model
open Opal

let input =
    LazyStream.of_channel (In_channel.open_bin "test-input")

let rooms_to_string rooms =
  RoomMap.to_seq rooms |> Seq.map snd |> List.of_seq |> List.to_string room_to_string

let start_room =
  match Parser.parse input with
  | None -> failwith "parsing failed"
  | Some rooms -> 
      let rooms' = Model.connect rooms in
      print_endline (rooms_to_string rooms');
      RoomMap.find "AA" rooms'

      (*
let rec has_moved from dest = function
  | dest' :: from' :: _ when dest' = dest && from' = from -> true
  | _ :: tail -> has_moved from dest tail
  | _ -> false

let rec max_pressure' path opened pressure minutes room =
  Printf.printf "%s = %d\n" (List.rev path |> List.string_list_to_string) pressure;
  if minutes <= 0 then begin
    pressure
  end else begin
    let is_open = Set.mem room.valve opened in
    let path' = room.valve :: path in
    let opened' = Set.add room.valve opened in
    let (minutes', pressure') =
      if room.pressure > 0 && not is_open then begin
        let m = minutes - 1 in
        (m, pressure + room.pressure * m)
      end else
        (minutes, pressure)
    in
    room.connections
    |> List.filter (fun dest -> has_moved room.valve dest.valve path |> not)
    |> List.map (max_pressure' path' opened' pressure' (minutes' - 1))
    |> List.max_opt Int.compare
    |> Option.default pressure
  end
*)


type path_visit = {
  path_valve: string;
  paths_open: room list
}

let visit_to_string visit = 
  Printf.sprintf "%s (%s)" visit.path_valve (List.map (fun r -> r.valve) visit.paths_open |> List.string_list_to_string)

(*
let any_opened_since valve visits =
  let rec any_opened_since' valve opened = function
    | [] -> false
    | {valve = valve'; paths_taken = _; _} :: _ when valve' = valve -> opened
    | {valve = _; paths_taken = _; did_open = opened'} :: path' -> any_opened_since' valve (opened' || opened) path'
  in
  any_opened_since' valve false visits

let rec paths_taken valve = function
  | [] -> []
  | {valve = valve'; paths_taken = paths_taken; _} :: _ when valve' = valve -> paths_taken
  | _ :: path' -> paths_taken valve path'

let remove_paths room_names (connections: room list) =
  connections |> List.filter (fun (room: room) -> Set.mem room.valve room_names)

let remove_skippable_rooms rooms = 
  if room.pressure = 0 then
    room.connections |> List.filter (fun r -> r.valve <> from)
  else
    room.connections
in

let path_visit_to_string p =
  Printf.sprintf "{%s; %s; %b}" p.valve (List.string_list_to_string p.paths_taken) p.did_open

let rec max_pressure' all_open path minutes from (room: room) =
  let path_list = path |> List.rev |> List.to_string path_visit_to_string in
  let paths_taken = paths_taken room.valve path in
  let paths_to_try = remove_paths (paths_taken |> Set.of_list) room.connections in
  let pressure =
    if minutes <= 0 || (any_opened_since room.valve path && paths_to_try = []) then begin
      Printf.printf "Exit: %s (%s)\n" path_list room.valve;
      0
    end else begin
      let is_open = Set.mem room.valve all_open in
      let do_open = room.pressure > 0 && not is_open in    blind = false

  pressure
  *)

let rec last_visit valve path =
  match path with
  | [] -> None
  | visit :: tail when visit.path_valve = valve -> Some visit
  | _ :: tail -> last_visit valve tail

let rec last_visit valve path =
  match path with
  | [] -> None
  | visit :: tail when visit.path_valve = valve -> Some visit
  | _ :: tail -> last_visit valve tail

let neighbours (room: room) visit =
  match visit with
  | Some visit -> visit.paths_open
  | None -> room.connections

let rec visit_rooms path minutes room descendents =
  match descendents with
    | [] -> []
    | descendent :: tail ->
        let path' = { path_valve = room.valve; paths_open = tail } :: path in
        max_pressure' path' minutes descendent :: visit_rooms path minutes room tail

and max_pressure' path minutes room =
  let last_visit = last_visit room.valve path in
  match neighbours room last_visit with
  | [] -> 0
  | paths_open -> begin
      let is_open = Option.is_none last_visit && room.pressure <> 0 in
      let (minutes', pressure) =
        if is_open then
          let minutes'' = minutes - 1 in
          (minutes'', room.pressure * minutes'')
        else
          (minutes, 0)
      in
      let pressure' = pressure + (visit_rooms path (minutes' - 1) room paths_open
      |> List.max_opt Int.compare
      |> Option.default 0) in
      Printf.printf "%s = %d\n" (List.rev path |> List.to_string visit_to_string) pressure';
      pressure'
    end

let start_path = { path_valve = start_room.valve; paths_open = start_room.connections } :: []

let max_pressure start_room =
  max_pressure' [] 30 start_room

let solve_part1 = max_pressure

let () =
  print_newline ();
  Printf.printf "Part 1, max pressure: %d\n" (max_pressure start_room)
