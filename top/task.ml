type status =
  | Paused of int64
  | Active of int64
  | Resolved of int64
  | Cancelled of int64

type t = {
    uid: int
  ; parent: int option
  ; runner: int
  ; resources: int
  ; start: int64
  ; status: status
}
