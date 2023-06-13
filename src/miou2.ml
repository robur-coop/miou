module Prm = struct
  type 'a state =
    | Pending of 'a t Tq.t
    | Resolved of 'a
    | Consumed of ('a, exn) result
    | Failed of exn

  and 'a t = { state: 'a state Atomic.t }

  let await t =
    match Atomic.get t.state with
    | Resolved v | Consumed (Ok v) -> Ok v
    | Failed exn | Consumed (Error exn) -> Error exn
    | Pending q -> Effect.perform (Await (t, q))
end

type process =
  | Task : 'a Prm.t * (unit -> 'a) -> process
  | Domain : domain * 'a Prm.t * ('a, exn) result Domain.t -> process
  | Sys : 'a Prm.t -> process
