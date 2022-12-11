open Batteries
open Extensions

type operation =
  | Plus of int
  | MultiplyOld
  | Multiply of int

let operation_to_string = function
| Plus n -> Printf.sprintf "old + %d" n
| Multiply n -> Printf.sprintf "old * %d" n
| MultiplyOld -> "old * old"

type monkey = {
  number: int;
  items: int list;
  operation: operation;
  divisible_by: int;
  when_true: int;
  when_false: int;
  inspected: int;
}

let monkey_to_string m =
  Printf.sprintf "{ number=%d; items=%s; operation=%s; divisible_by=%d; when_true=%d; when_false=%d; inspected=%d }"
    m.number
    (List.int_list_to_string m.items)
    (operation_to_string m.operation)
    m.divisible_by
    m.when_true
    m.when_false
    m.inspected
