type room = {
  valve: string;
  pressure: int;
  connection_names: string list;
  mutable connections: room list;
  mutable blind: bool
}


module RoomMap = Map.Make(String)


let connect rooms =
  let room_map = List.map (fun v -> (v.valve, v)) rooms |> List.to_seq |> RoomMap.of_seq in
  RoomMap.iter (fun _ v -> v.connections <- List.map (Fun.flip RoomMap.find room_map) v.connection_names) room_map;
  room_map


let mark_room room =
  if not room.blind then begin
    room.blind <- List.filter (fun r -> not r.blind) room.connections |> List.length = 1;
    room.blind
  end else
    false

let mark_rooms rooms =
  let marked = ref false in
  List.iter (fun room -> marked := !marked || mark_room room) rooms;
  !marked

let mark_blind rooms =
  while mark_rooms rooms do () done; rooms
