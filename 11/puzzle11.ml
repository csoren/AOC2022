open Batteries
open Extensions
open Model
open Opal

let input =
  LazyStream.of_channel (In_channel.open_bin "puzzle-input")

let increase_worry old = function
| MultiplyOld -> old * old
| Multiply n -> old * n
| Plus n -> old + n

let one_monkey m =
  let one_monkey_item (t, f) n = 
    let n' = increase_worry n m.operation / 3 in
    if n' mod m.divisible_by = 0 then (n' :: t, f)
    else (t, n' :: f)
  in
  List.fold one_monkey_item ([], []) m.items

let add_items m i = { m with items = m.items @ i }

let one_round monkeys =
  for i = 0 to Array.length monkeys - 1 do
    let m = monkeys.(i) in
    let (t, f) = one_monkey m in
    monkeys.(i) <- { m with items = []; inspected = m.inspected + List.length m.items };
    monkeys.(m.when_true) <- add_items monkeys.(m.when_true) t;
    monkeys.(m.when_false) <- add_items monkeys.(m.when_false) f;
  done

let do_rounds n m =
  let m' = Array.of_list m in
  for _ = 1 to n do
    one_round m'
  done;
  Array.to_list m'
  
let solve m =
  let m' = do_rounds 20 m in
  let m'' = List.sort (fun m1 m2 -> m2.inspected - m1.inspected) m' in
  List.iter (monkey_to_string %> print_endline) m'';
  let monkey_business = List.take 2 m'' |> List.map (fun m -> m.inspected) |> List.reduce ( * ) in
  Printf.printf "Monkey business: %d\n" monkey_business


let () =
  print_newline ();
  match Parser.parse input with
  | None -> failwith "Parsing failed"
  | Some monkeys -> solve monkeys
