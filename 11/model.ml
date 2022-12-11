open Batteries
open Extensions

type operation =
  | Plus of Big_int.t
  | MultiplyOld
  | Multiply of Big_int.t

let operation_to_string = function
| Plus n -> Printf.sprintf "old + %s" (Big_int.to_string n)
| Multiply n -> Printf.sprintf "old * %s" (Big_int.to_string n)
| MultiplyOld -> "old * old"

type monkey = {
  number: int;
  items: Big_int.t list;
  operation: operation;
  divisible_by: Big_int.t;
  when_true: int;
  when_false: int;
  inspected: int;
}

let monkey_to_string m =
  Printf.sprintf "{ number=%d; items=%s; operation=%s; divisible_by=%s; when_true=%d; when_false=%d; inspected=%d }"
    m.number
    (List.to_string (Big_int.to_string) m.items)
    (operation_to_string m.operation)
    (Big_int.to_string m.divisible_by)
    m.when_true
    m.when_false
    m.inspected
