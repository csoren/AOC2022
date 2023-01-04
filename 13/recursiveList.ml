open Extensions
open Opal

type t =
  | Int of int
  | List of t list

let rec compare lhs rhs =
  match (lhs, rhs) with
    | ( Int l,  Int r) -> Int.compare l r
    | (List l, List r) -> compare_lists l r
    | (List l,      r) -> compare_lists l [r]
    | (     l, List r) -> compare_lists [l] r
and compare_lists ll rl =
  match (ll, rl) with
    | ([], []) -> 0
    | ([],  _) -> -1
    | (_,  []) -> 1
    | (l :: tl, r :: tr) -> 
        let c = compare l r in
        if c = 0 then compare_lists tl tr
        else c


let number =
  spaces >> many1 digit => fun s -> Int (s |> String.implode |> int_of_string)

let rec list_item input =
  (recursive_list <|> number) input
and list_body input =
  (sep_by list_item (token ",")) input
and recursive_list input = 
  (lexeme (token "[") >> list_body << lexeme (token "]") => fun l -> List l) input
  
let pair =
  recursive_list >>= fun list1 ->
  recursive_list >>= fun list2 ->
  return (list1, list2)
  
let parse =
  many1 pair

let of_file file =
  Opal.parse parse @@ LazyStream.of_channel (In_channel.open_text file)
