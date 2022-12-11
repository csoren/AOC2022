open Batteries
open Model
open Opal

let digits = spaces >> many1 digit => String.implode => int_of_string

let number_list = sep_by1 digits (token ",")

let multiply_int = token "*" >> digits => fun n -> Multiply n

let plus_int = token "+" >> digits => fun n -> Plus n

let multiply_old = token "*" >> spaces >> token "old" => fun _ -> MultiplyOld

let operation =
  token "new" >> spaces >> token "=" >> spaces >> token "old" >> spaces >>
  ( multiply_int <|> multiply_old <|> plus_int )

let operation_line = spaces >> token "Operation:" >> spaces >> operation 

let headline = token "Monkey" >> digits << token ":"

let items_line = spaces >> token "Starting items:" >> spaces >> number_list

let test_line = spaces >> token "Test:" >> spaces >> token "divisible by" >> digits

let if_line clause = spaces >> token "If" >> spaces >> token clause >> token ":" >> spaces >> token "throw to monkey" >> digits

let monkey =
  headline >>= fun number ->
  items_line >>= fun items ->
  operation_line >>= fun operation ->
  test_line >>= fun divisible_by ->
  if_line "true" >>= fun when_true ->
  if_line "false" >>= fun when_false ->
  return {
    number;
    items;
    operation;
    divisible_by;
    when_true;
    when_false
  }

let monkey_list =
  many1 monkey

let parse = Opal.parse monkey_list
