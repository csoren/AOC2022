type square_kind =
  | Start
  | End
  | Regular

type square = {
  kind: square_kind;
  height: int;
  blocked: bool;
}
