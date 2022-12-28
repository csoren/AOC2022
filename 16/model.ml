open Extensions

type room = {
  valve: string;
  pressure: int;
  connection_names: string list;
  mutable connections: room list;
  mutable blind: bool
}


let room_to_string room =
  Printf.sprintf "{ valve=%s; pressure=%d; connections=%s; blind=%b }\n" room.valve room.pressure (room.connections |> List.map (fun v -> v.valve) |> List.string_list_to_string ) room.blind


module RoomMap = Map.Make(String)


let mark_room room =
  if not room.blind then begin
    room.blind <- List.count_matching (fun r -> not r.blind) room.connections = 1;
    room.blind
  end else
    false

let mark_rooms rooms =
  let marked = ref false in
  RoomMap.iter (fun _ room -> marked := mark_room room || !marked) rooms;
  !marked

let mark_blind rooms =
  while mark_rooms rooms do () done; rooms


let connect rooms =
  let room_map = List.map (fun v -> (v.valve, v)) rooms |> List.to_seq |> RoomMap.of_seq in
  RoomMap.iter (fun _ v -> v.connections <- List.map (Fun.flip RoomMap.find room_map) v.connection_names) room_map;
  room_map
  
  
  