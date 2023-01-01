open Batteries
open Extensions

let input =
  File.lines_of "test-input" |> List.of_enum |> List.hd

let string_to_block_line s =
  let line = 
    String.explode s |> List.rev
    |> List.fold (fun acc el -> acc lsr 1 lor ((el = '#' |> Bool.to_int) lsl 6)) 0
  in
  line lsr 2

let string_list_to_block l =
  let block = List.map string_to_block_line l in
  let pad = 4 - (List.length block) in
  List.append (List.make pad 0) block

let blocks =
  File.lines_of "blocks" |> List.of_enum
  |> List.group_at ~separator:String.is_empty
  |> List.map string_list_to_block

type instruction = Left | Right

let char_to_instruction = function
  | '<' -> Left
  | '>' -> Right
  | _ -> failwith "this never happens"

let instructions =
  input |> String.explode |> List.map char_to_instruction

let decimal_to_binary_string d =
  List.range 6 `Downto 0 |> List.map (fun bit -> if (d land (1 lsl bit)) <> 0 then '1' else '0') |> String.of_list

let block_to_string block =
  List.map decimal_to_binary_string block |> String.concat "\n"

let print_blocks =
  List.iter (block_to_string %> Printf.printf "%s\n\n")

let initial_field =
  [ 0x7F ]

let prepare_field =
  List.append (List.make 7 0)

let can_place_block block field =
  (* let field_top = List.take 4 field in
  print_endline (List.int_list_to_string field_top);
  print_endline (List.int_list_to_string block); *)
  let mask = List.map2 (land) block (List.take 4 field) in
  (* print_endline (List.int_list_to_string mask); *)
  mask |> List.for_all ((=) 0)

let push_block column_mask op block =
  let can_move = List.map ((land) column_mask) block |> List.for_all ((=) 0) in
  if can_move then
    List.map (fun d -> op d 1) block
  else
    block

let push_block_left =
  push_block 0x40 (lsl)

let push_block_right =
  push_block 0x01 (lsr)

let push_block = function
  | Left -> push_block_left
  | Right -> push_block_right

let move_block_direction instruction block field =
  let block' = push_block instruction block in
  if can_place_block block' field then begin
    (* print_endline "Horizontal"; *)
    block'
  end else
    block

let move instruction block field =
  let block' = move_block_direction instruction block field in
  match field with
    | top :: rest ->
        if can_place_block block' rest then begin
          (* print_endline "Vertical"; *)
          (true, block')
        end else
          (false, block')
    | _ -> failwith "this never happens"

let merge_block_with_field block field =
  let field_head = List.take 4 field in
  let field_tail = List.drop 4 field in
  List.append (List.map2 (lor) field_head block) field_tail

let drop_block instructions block field =
  let rec drop' instructions block field_head field_tail =
    if Seq.is_empty instructions then (instructions, field)
    else
    (* let field' = (List.append (List.rev field_head) (merge_block_with_field block field_tail)) in
    Printf.printf "%s\n\n" (block_to_string field'); *)
    let instruction = Seq.hd instructions in
    let instructions' = Seq.tl instructions in
    let (moved, block') = move instruction block field_tail in
    if moved then
      drop' instructions' block' (List.hd field_tail :: field_head) (List.tl field_tail)
    else begin
      let field' = (List.append (List.rev field_head) (merge_block_with_field block' field_tail)) in
      (instructions', field')
    end
  in
  drop' instructions block [] field

let rec drop_blocks n instructions blocks field =
  if n > 0 then
    let (instructions', field') = drop_block instructions (Seq.hd blocks) field in
    let field'' = List.drop_while ((=) 0) field' in
    drop_blocks (n - 1) instructions' (Seq.tl blocks) (prepare_field field'')
  else
    (field, List.drop_while ((=) 0) field |> List.length |> Fun.flip (-) 1)

let solve_instructions instructions =
  drop_blocks (List.length instructions) (Seq.of_list instructions) (Seq.cycle (Seq.of_list blocks)) (prepare_field initial_field)

let solve_part1 () =
  (* let (field, height) = solve_instructions instructions in
  Printf.printf "%s\n\n" (block_to_string field); *)
  let (_, height) = drop_blocks 2022 (Seq.cycle (Seq.of_list instructions)) (Seq.cycle (Seq.of_list blocks)) (prepare_field initial_field) in
  height

let solve_part2 () =
  drop_blocks 1000000000000 (Seq.cycle (Seq.of_list instructions)) (Seq.cycle (Seq.of_list blocks)) (prepare_field initial_field)

let () =
  print_newline ();
  print_blocks blocks;
  Printf.printf "Part 1, height = %d\n" (solve_part1 ());
  (* flush stdout;
  Printf.printf "Part 2, height = %d\n" (solve_part2 ()) *)
