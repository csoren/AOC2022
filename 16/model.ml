open Extensions

type room = {
  valve: string;
  pressure: int;
  mutable connection_names: string list;
  mutable connections: room list;
}


let room_to_string room =
  Printf.sprintf "{ valve=%s; pressure=%d; connections=%s }\n" room.valve room.pressure (room.connections |> List.map (fun v -> v.valve) |> List.string_list_to_string )


module RoomMap = Map.Make(String)

(*
let room_is_leaf room =
  List.length room.connections = 1

let room_descendent from room =
  match room.connections |> List.filter (fun r -> r.valve <> from) with
  | room' :: [] -> Some room'
  | _ -> None

let rec room_is_blind from room =
  match room_descendent from room with
  | Some room' -> room_is_leaf room' || room_is_blind room'.valve room'
  | None -> false

let replace_connection_to_this room valve =
  room.connections <- List.map (fun r -> if r.valve = valve then room else r) room.connections

let remove_unnecessary_descendent from room =
  match room_descendent from room with
  | Some descendent when room.pressure = 0 -> begin
      room.connections <- descendent.connections;
      room.move_cost <- room.move_cost + descendent.move_cost;
      replace_connection_to_this room descendent.valve;
      true
    end
  | _ -> false
*)

(*
let replace_valve room remove insert =
  room.connection_names <- List.map (fun v -> if v = remove then insert else v) room.connection_names; ()

let remove_room room rooms =
  match room.connection_names with
  | valve1 :: valve2 :: [] when room.pressure = 0 -> begin
      replace_valve (RoomMap.find valve1 rooms) room.valve valve2;
      replace_valve (RoomMap.find valve2 rooms) room.valve valve1;
      true
    end
  | _ -> false


let rec remove_zero_pressure rooms =
  let room_map = rooms |> List.to_seq |> Seq.map (fun r -> (r.valve, r)) |> RoomMap.of_seq in
  if RoomMap.for_all (fun _ room -> remove_room room room_map |> not) room_map |> not then begin
    let connected_rooms = RoomMap.to_seq room_map |> List.of_seq |> List.flat_map (fun (_, r) -> r.connection_names) |> Set.of_list in
    remove_zero_pressure (List.filter (fun r -> Set.mem r.valve connected_rooms) rooms)
  end
*)

let connect rooms =
  let room_map = List.map (fun v -> (v.valve, v)) rooms |> List.to_seq |> RoomMap.of_seq in
  RoomMap.iter (fun _ v -> v.connections <- List.map (Fun.flip RoomMap.find room_map) v.connection_names) room_map;
  room_map
  
  
  