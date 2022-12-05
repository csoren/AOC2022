open Batteries

include String

let drop n s =
  String.sub s n (String.length s - n)

let take_while f s =
  String.to_list s |> List.take_while f |> String.of_list

let drop_while f s =
  String.to_list s |> List.drop_while f |> String.of_list
