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
      let rooms' = Model.connect rooms |> Model.mark_blind in
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
  valve: string;
  paths_taken: string list;
  did_open: bool
}

let any_opened_since valve visits =
  let rec any_opened_since' valve opened = function
    | [] -> false
    | {valve = valve'; paths_taken = _; _} :: _ when valve' = valve -> opened
    | {valve = _; paths_taken = _; did_open = opened'} :: path' -> any_opened_since' valve (opened' || opened) path'
  in
  any_opened_since' valve false visits

let path_visit_to_string p =
  Printf.sprintf "{%s; %s; %b}" p.valve (List.string_list_to_string p.paths_taken) p.did_open

let rec max_pressure' all_open path minutes from room =
  let path_list = path |> List.rev |> List.to_string path_visit_to_string in
  let pressure =
    if minutes <= 0 || any_opened_since room.valve path then begin
      Printf.printf "Exit: %s (%s)\n" path_list room.valve;
      0
    end else begin
      let is_open = Set.mem room.valve all_open in
      let do_open = room.pressure > 0 && not is_open in
      let minutes' = if do_open then minutes - 1 else minutes in
      let all_open' = Set.add room.valve all_open in
      let path' = (room.valve, do_open) :: path in
      let opened_pressure =
        if do_open then
          minutes' * room.pressure
        else
          0
      in
      let neighbours = 
        if room.pressure = 0 then
          room.connections |> List.filter (fun r -> r.valve <> from)
        else
          room.connections
      in
      opened_pressure + (List.map (max_pressure' all_open' path' (minutes' - 1) room.valve) neighbours
      |> List.max_opt Int.compare
      |> Option.default 0)
    end
  in
  Printf.printf "%s (%s) = %d\n" path_list room.valve pressure;
  flush stdout;
  pressure

let max_pressure start_room =
  max_pressure' Set.empty [] 30 "" start_room

let solve_part1 = max_pressure

let () =
  print_newline ();
  Printf.printf "Part 1, max pressure: %d\n" (max_pressure start_room)
