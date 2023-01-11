open Batteries
open Extensions

let push_block instruction block field =
  let block' = Block.push instruction block in
  if Field.can_place_block block' field then block'
  else block


let move instruction block field =
  let block' = push_block instruction block field in
  match field with
    | top :: rest when Field.can_place_block block' rest ->
        (true, block')
    | _ ->
        (false, block')


let drop_block instructions block field =
  let rec drop' block field_head field_tail instructions =
    let instructions' = Seq.tl instructions in
    match move (Seq.hd instructions) block field_tail with
      | (true, block') -> 
          drop' block' (List.hd field_tail :: field_head) (List.tl field_tail) instructions'
      | (false, block') ->
          let field' = (List.rev field_head) @ (Field.merge_block block' field_tail) in
          (instructions', field')
  in
  drop' block [] field instructions


let drop_blocks n = 
  let rec drop_blocks' n instructions blocks field =
    if n > 0 && (not @@ Seq.is_empty instructions) then
      let (instructions', field') = drop_block instructions (Seq.hd blocks) field in
      drop_blocks' (n - 1) instructions' (Seq.tl blocks) (Field.prepare field')
    else
      let field' = Field.trim_top field |> List.rdrop 1 in
      (field', List.length field')
  in
  drop_blocks' n Instruction.cycle Block.cycle (Field.prepare Field.initial)

  
let solve_part1 () =
  snd @@ drop_blocks 2022


let compare_cycles index length a =
  List.range 0 `To (length - 1) |> List.for_all (fun i -> a.(index + i) = a.(length + index + i))


let rec find_tower_cycle () =
  let match_size = 32 in
  let (field', _) = drop_blocks 10000 in
  let index = Random.int (List.length field' - match_size) in
  let field'' = List.rev field' |> Array.of_list in
  let block = Array.sub field'' index match_size in
  match Array.find_subs block field'' with
    | i1 :: i2 :: _ -> begin
        let length = i2 - i1 in
        match List.range 0 `To (Array.length field'' - length * 2) |> List.find_opt (fun i -> compare_cycles i length field'') with
          | Some index -> (index + length, length)
          | None -> find_tower_cycle ()
      end
    | _ -> find_tower_cycle ()


let blocks_to_reach_height n =
  let rec blocks_to_reach_height' n instructions blocks field count =
    let (instructions', field) = drop_block instructions (Seq.hd blocks) field in
    let field' = Field.trim_top field in
    let height = List.length field' - 1 in
    if height >= n then
      count
    else
      blocks_to_reach_height' n instructions' (Seq.tl blocks) (Field.prepare field') (count + 1)
  in
  blocks_to_reach_height' n Instruction.cycle Block.cycle (Field.prepare Field.initial) 1


let solve_part2 () =
  let drop_blocks n = snd @@ drop_blocks n in
  let blocks_to_drop = 1000000000000 in
  let (lines_bottom, lines_cycle_length) = find_tower_cycle () in
  (* Now we know how long the cycle of lines is and where it is.
     Do a lot of slightly confusing but actually very simple math
     to find out how tall the tower will be *)
  let blocks_bottom = blocks_to_reach_height lines_bottom in
  let blocks_cycle_length = blocks_to_reach_height (lines_bottom + lines_cycle_length) - blocks_bottom in
  let cycles = (blocks_to_drop - blocks_bottom) / blocks_cycle_length in
  let blocks_top = blocks_to_drop - (blocks_bottom + cycles * blocks_cycle_length) in
  let lines_top = drop_blocks (blocks_bottom + blocks_cycle_length + blocks_top) - (lines_bottom + lines_cycle_length) in
  lines_bottom + cycles * lines_cycle_length + lines_top


let () =
  Random.self_init ();
  print_newline ();
  Printf.printf "Part 1, height = %d\n" (solve_part1 ());
  Printf.printf "Part 2, height = %d\n" (solve_part2 ())
