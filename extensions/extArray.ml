open Batteries

include Array

let get_set f a n =
  Array.set a n (f (Array.get a n))

