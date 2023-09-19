type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

let effc eff k = Suspended (k, eff)

let handler_continue =
  let open Effect.Shallow in
  let retc value = Finished (Ok value) in
  let exnc exn = Finished (Error exn) in
  let effc :
      type c. c Effect.t -> ((c, 'a) Effect.Shallow.continuation -> 'b) option =
   fun effect -> Some (effc effect)
  in
  { retc; exnc; effc }

let continue_with : ('c, 'a) continuation -> 'c -> 'a t =
 fun k v -> Effect.Shallow.continue_with k v handler_continue

let handler_discontinue exn =
  let open Effect.Shallow in
  let effc :
      type c. c Effect.t -> ((c, 'a) Effect.Shallow.continuation -> 'b) option =
    function
    | _ -> Some (Fun.const (Finished (Error exn)))
  in
  let retc = Fun.const (Finished (Error exn)) in
  let exnc = Fun.const (Finished (Error exn)) in
  { retc; exnc; effc }

let discontinue_with : ('c, 'a) continuation -> exn -> 'a t =
 fun k exn -> Effect.Shallow.discontinue_with k exn (handler_discontinue exn)

let suspended_with : ('c, 'a) continuation -> 'c Effect.t -> 'a t =
 fun k e -> Suspended (k, e)

let unhandled_with : ('c, 'a) continuation -> 'c -> 'a t =
 fun k v -> Unhandled (k, v)

let pure res = Finished res

let make k v =
  let k = Effect.Shallow.fiber k in
  continue_with k v

type 'a step =
  | Send of 'a
  | Fail of exn
  | Intr
  | Cont : 'a Effect.t -> 'a step
  | None : 'a Effect.t -> 'a step
  | Yield : unit step

type ('a, 'b) k = ('a step -> 'b t) -> 'a Effect.t -> 'b t
type perform = { perform: 'a 'b. ('a, 'b) k } [@@unboxed]

let once : type a. perform:perform -> a t -> a t =
 fun ~perform -> function
  | Finished _ as finished -> finished
  | Unhandled (fn, v) -> continue_with fn v
  | Suspended (fn, e) as state ->
      let k : type c. (c, a) continuation -> c step -> a t =
       fun fn -> function
        | Send v -> continue_with fn v
        | Fail exn -> discontinue_with fn exn
        | Intr -> state
        | Cont e -> suspended_with fn e
        | None eff ->
            let v = Effect.perform eff in
            unhandled_with fn v
        | Yield -> continue_with fn ()
      in
      perform.perform (k fn) e

exception Break

let is_finished = function Finished _ -> true | _ -> false

[@@@warning "-8"]

let run : type a. quanta:int -> perform:perform -> a t -> a t =
 fun ~quanta ~perform state ->
  let exception Yield of a t in
  let k : type c. (c, a) continuation -> c step -> a t =
   fun fn -> function
    | Send v -> continue_with fn v
    | Fail e -> discontinue_with fn e
    | Cont e -> suspended_with fn e
    | None e ->
        let v = Effect.perform e in
        unhandled_with fn v
    | Intr -> raise_notrace Break
    | Yield -> raise_notrace (Yield (continue_with fn ()))
  in
  let quanta = ref quanta and state = ref state in
  try
    while !quanta > 0 && is_finished !state = false do
      match !state with
      | Suspended (fn, e) ->
          state := perform.perform (k fn) e;
          quanta := !quanta - 1
      | Unhandled (fn, v) ->
          state := continue_with fn v;
          quanta := !quanta - 1
    done;
    !state
  with
  | Break -> !state
  | Yield state -> state

(* Please, don't... *)

[@@@warning "+8"]

let fail ~exn = function
  | Finished _ -> Finished (Error exn)
  | Suspended (k, _) -> discontinue_with k exn
  | Unhandled (k, _) -> discontinue_with k exn
