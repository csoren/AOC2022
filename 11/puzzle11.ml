open Batteries
open Extensions
open Model
open Opal

let input =
  LazyStream.of_channel (In_channel.open_bin "test-input")

let increase_worry old = function
| MultiplyOld -> Big_int.(old * old)
| Multiply n -> Big_int.(old * n)
| Plus n -> Big_int.(old + n)

let one_monkey worry_divide m =
  let one_monkey_item (t, f) n = 
    let n' = Big_int.(increase_worry n m.operation / worry_divide) in
    if Big_int.(equal (modulo n' m.divisible_by) zero) then (n' :: t, f)
    else (t, n' :: f)
  in
  List.fold one_monkey_item ([], []) m.items

let add_items m i = { m with items = m.items @ i }

let one_round worry_divide monkeys =
  for i = 0 to Array.length monkeys - 1 do
    let m = monkeys.(i) in
    let (t, f) = one_monkey worry_divide m in
    monkeys.(i) <- { m with items = []; inspected = m.inspected + List.length m.items };
    monkeys.(m.when_true) <- add_items monkeys.(m.when_true) t;
    monkeys.(m.when_false) <- add_items monkeys.(m.when_false) f;
  done

let do_rounds worry_divide n m =
  let m' = Array.of_list m in
  for _ = 1 to n do
    one_round worry_divide m'
  done;
  Array.to_list m'
  
let solve_part worry_divide rounds m =
  let m' = do_rounds (Big_int.of_int worry_divide) rounds m in
  let m'' = List.sort (fun m1 m2 -> m2.inspected - m1.inspected) m' in
  List.iter (monkey_to_string %> print_endline) m'';
  let monkey_business = List.take 2 m'' |> List.map (fun m -> m.inspected) |> List.reduce ( * ) in
  Printf.printf "Monkey business: %d\n" monkey_business

let solve m =
  print_endline "Part 1:";
  solve_part 3 20 m;
  print_endline "Part 2:";
  solve_part 1 100 m

let () =
  print_newline ();
  match Parser.parse input with
  | None -> failwith "Parsing failed"
  | Some monkeys -> solve monkeys
