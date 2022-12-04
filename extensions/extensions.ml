module List = struct
  open Batteries
  include List

  let group_at ~separator:sep l =
    List.nsplit sep l |> List.filter (not % List.is_empty)

end
