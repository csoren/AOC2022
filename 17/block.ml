open Batteries
open Extensions

let string_to_block_line s =
  let line = 
    String.explode s |> List.rev
    |> List.fold (fun acc el -> acc lsr 1 lor ((el = '#' |> Bool.to_int) lsl 6)) 0
  in
  line lsr 2

let string_list_to_block l =
  let block = List.map string_to_block_line l in
  let pad = 4 - (List.length block) in
  (List.make pad 0) @ block

let decimal_to_binary_string d =
  List.range 6 `Downto 0 |> List.map (fun bit -> if (d land (1 lsl bit)) <> 0 then '1' else '0') |> String.of_list

let to_string block =
  List.map decimal_to_binary_string block |> String.concat "\n"

let print =
  List.iter (to_string %> Printf.printf "%s\n\n")
  
  let push_block column_mask op block =
    let can_move = List.map ((land) column_mask) block |> List.for_all ((=) 0) in
    if can_move then
      List.map (fun d -> op d 1) block
    else
      block
  
let push_left =
  push_block 0x40 (lsl)

let push_right =
  push_block 0x01 (lsr)

let push = function
  | Instruction.Left -> push_left
  | Instruction.Right -> push_right

    
let all =
  File.lines_of "blocks" |> List.of_enum
  |> List.group_at ~separator:String.is_empty
  |> List.map string_list_to_block

let cycle =
  Seq.cycle (Seq.of_list all)