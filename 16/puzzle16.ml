open Batteries
open Extensions
open Model
open Opal

let input =
    LazyStream.of_channel (In_channel.open_bin "test-input")

let rooms_to_string rooms =
  RoomMap.to_seq rooms |> Seq.map snd |> List.of_seq |> List.to_string room_to_string

let rooms =
  match Parser.parse input with
  | None -> failwith "parsing failed"
  | Some rooms -> Model.connect rooms

let start_room =
  RoomMap.find "AA" rooms

let valve_rooms =
  RoomMap.to_seq rooms |> Seq.filter (fun (_, r) -> r.pressure <> 0) |> Seq.map snd |> List.of_seq

let distance_iterate distances =
  let neighbour_distance cell = Map.String.find cell distances |> Option.map (( + ) 1) in
  let distance room = List.flatmap_opt neighbour_distance room.connection_names |> List.hd_opt in
  Map.String.to_seq distances |> Seq.map (fun (v, d) -> Option.default_delayed (RoomMap.find v rooms |> distance))

let distance_to_all_valves valve: int Map.String.t =
  Map.String.empty

let valve_distances =
  List.map (fun r -> (r.valve, distance_to_all_valves r)) valve_rooms |> List.to_seq |> Map.String.of_seq

let distance_to from dest =
  Map.String.find from.valve valve_distances |> Map.String.find dest.valve

let rec max_pressure' opened minutes room =
  if minutes <= 0 then 0
  else
    let closed_valves = valve_rooms |> List.filter (fun v -> Set.mem v.valve opened |> not) in
    let opened' = Set.add room.valve opened in
    List.map (fun dest -> max_pressure' opened' (distance_to room dest + minutes + 1) dest) closed_valves
    |> List.max_opt Int.compare
    |> Option.default 0

let max_pressure start_room =
  max_pressure' Set.empty 30 start_room

let solve_part1 = max_pressure

let () =
  print_newline ();
  Printf.printf "Part 1, max pressure: %d\n" (max_pressure start_room)
