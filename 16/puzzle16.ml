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
  start_room :: (RoomMap.to_seq rooms |> Seq.filter (fun (_, r) -> r.pressure <> 0) |> Seq.map snd |> List.of_seq)

(* Distance map *)  
type distance = (int option * room)

let option_to_string = function
| None -> "None"
| Some s -> Printf.sprintf "Some %d" s

let distance_to_string (d, r) =
  Printf.sprintf "(%s,%s)" r.valve (option_to_string d)

let neighbours_distances distances room =
  let neighbours = List.map (Fun.flip Map.String.find distances) room.connection_names in
  List.map fst neighbours

let neighbour_distance distances room =
  let d = neighbours_distances distances room |> List.find_opt Option.is_some |> Option.default None in
  d

let distance_iterate distances =
  let increment_distance (d, room) = 
    match d with
    | None -> neighbour_distance distances room |> (fun d -> (Option.map ((+) 1) d, room))
    | d -> (d, room)
  in
  let r = Map.String.map increment_distance distances in
  (* print_endline (r |> Map.String.to_seq |> Seq.map snd |> List.of_seq |> List.to_string distance_to_string); *)
  r


let distance_to_all_valves valve: int Map.String.t =
  (* Printf.printf "Distances to %s\n" valve; *)
  flush stdout;
  let rec distances d =
    let d' = distance_iterate d in
    let is_done = valve_rooms |> List.map (fun v -> v.valve) |> List.for_all (fun v -> Map.String.find v d' |> fst |> Option.is_some) in
    if is_done then d'
    else distances d'
  in
  Map.String.map (fun room -> ((if room.valve = valve then Some 0 else None), room)) rooms |> distances |> Map.String.map (fst %> Option.get)

let valve_distances =
  List.map (fun r -> (r.valve, distance_to_all_valves r.valve)) valve_rooms |> List.to_seq |> Map.String.of_seq

let distance_to from dest =
  (* print_endline ("Distance " ^ from.valve ^ "->" ^ dest.valve); *)
  Map.String.find from.valve valve_distances |> Map.String.find dest.valve

let rec max_pressure' path pressure opened minutes room =
  if minutes <= 0 then begin
    (* Printf.printf "%s = %d\n" path pressure; *)
    pressure
  end else
    let minutes' = minutes - 1 in
    let pressure' = pressure + room.pressure * minutes' in
    let opened' = Set.add room.valve opened in
    let closed_valves = List.filter (fun v -> Set.mem v.valve opened' |> not) valve_rooms in
    let r =
      List.map (fun dest -> max_pressure' (path ^ ">" ^ dest.valve) pressure' opened' (minutes' - (distance_to room dest)) dest) closed_valves
      |> List.max_opt Int.compare
      |> Option.default pressure'
    in
    r

module Part1 = struct
  (* Best path part 1 *)  
  let max_pressure start_room =
    max_pressure' "AA" 0 (Set.singleton "AA") (30 + 1) start_room

  let solve () = max_pressure start_room
end

module Part2 = struct
  (* Best path part 2 *)  
  let rec max_pressure'' path pressure opened (minutes, room) (other_minutes, other_room) =
    if minutes < other_minutes then
      max_pressure'' path pressure opened (other_minutes, other_room) (minutes, room)
    else begin
      let minutes' = minutes - 1 in
      let pressure' = pressure + room.pressure * minutes' in
      let opened' = Set.add room.valve opened in
      let closed_valves = List.filter (fun v -> Set.mem v.valve opened' |> not) valve_rooms in
      let r =
        List.map (fun dest -> max_pressure'' (path ^ ">" ^ dest.valve) pressure' opened' (minutes' - (distance_to room dest), dest) (other_minutes, other_room)) closed_valves
        |> List.max_opt Int.compare
        |> Option.default pressure'
      in
      r
    end

  let max_pressure start_room =
    max_pressure'' "AA" 0 (Set.singleton "AA") (26 + 1, start_room) (26 + 1, start_room)

  let solve () = max_pressure start_room
end


let () =
  print_newline ();
  Printf.printf "Part 1, max pressure: %d\n" (Part1.solve ());
  Printf.printf "Part 2, max pressure: %d\n" (Part2.solve ())
