open Batteries
open Extensions
open Model
open Opal

let input =
    LazyStream.of_channel (In_channel.open_bin "test-input")
  
let start_room =
  match Parser.parse input with
  | None -> failwith "parsing failed"
  | Some rooms -> 
      let rooms' = Model.mark_blind rooms |> Model.connect in
      RoomMap.find "AA" rooms'


let rec max_pressure' opened pressure minutes room =
  let is_open = Set.mem room.valve opened in
  if minutes <= 0 then pressure
  else if is_open && room.blind then pressure
  else begin
    let opened' = Set.add room.valve opened in
    let (minutes', pressure') =
      if (not is_open && room.pressure > 0) then
        let m = minutes - 1 in
        (m, pressure + room.pressure * m)
      else
        (minutes, pressure)
    in
    List.map (max_pressure' opened' pressure' (minutes' - 1)) room.connections
    |> List.max ~cmp:Int.compare

  end

let max_pressure start_room =
  max_pressure' Set.empty 0 30 start_room

let solve_part1 = max_pressure

let () =
  print_newline ();
  Printf.printf "Part 1, max pressure: %d\n" (max_pressure start_room)
