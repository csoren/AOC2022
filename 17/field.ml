open Extensions

let prepare field =
  let field' = List.drop_while ((=) 0) field in
  (List.make 7 0) @ field'

  
let initial =
  [ 0x7F ]
  
  
let can_place_block block field =
  let mask = List.map2 (land) block (List.take 4 field) in
  mask |> List.for_all ((=) 0)


let merge_block block field =
  let (field_head, field_tail) = List.takedrop 4 field in
  (List.map2 (lor) field_head block) @ field_tail
  
  
let trim_top =
  List.drop_while ((=) 0) 
