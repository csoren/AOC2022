module BatList = struct
  open Batteries
  include BatList

  let group_at ~separator:sep l =
    BatList.nsplit sep l |> BatList.filter (not % BatList.is_empty)

end