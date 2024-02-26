type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation
type error = Printexc.raw_backtrace * exn

type 'a t =
  | Finished of ('a, error) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

let retc value = Finished (Ok value)

let exnc exn =
  let bt = Printexc.get_raw_backtrace () in
  Finished (Error (bt, exn))

let effc eff k = Suspended (k, eff)

let handler_continue =
  let open Effect.Shallow in
  let effc :
      type c. c Effect.t -> ((c, 'a) Effect.Shallow.continuation -> 'b) option =
   fun effect -> Some (effc effect)
  in
  { retc; exnc; effc }

let continue_with : ('c, 'a) continuation -> 'c -> 'a t =
 fun k v -> Effect.Shallow.continue_with k v handler_continue

let handler_discontinue bt_and_exn =
  let open Effect.Shallow in
  let const _ = Finished (Error bt_and_exn) in
  let effc :
      type c. c Effect.t -> ((c, 'a) Effect.Shallow.continuation -> 'b) option =
   fun _ -> Some const
  and retc = const
  and exnc = const in
  { retc; exnc; effc }

let discontinue_with :
    backtrace:Printexc.raw_backtrace -> ('c, 'a) continuation -> exn -> 'a t =
 fun ~backtrace:bt k exn ->
  Effect.Shallow.discontinue_with_backtrace k exn bt
    (handler_discontinue (bt, exn))

let suspended_with : ('c, 'a) continuation -> 'c Effect.t -> 'a t =
 fun k e -> Suspended (k, e)

let unhandled_with : ('c, 'a) continuation -> 'c -> 'a t =
 fun k v -> Unhandled (k, v)

let pure res = Finished res

let make k v =
  let k = Effect.Shallow.fiber k in
  continue_with k v

module Op = struct
  type 'a t =
    | Return of 'a
    | Fail of Printexc.raw_backtrace * exn
    | Interrupt
    | Continue : 'a Effect.t -> 'a t
    | Perform : 'a Effect.t -> 'a t
    | Yield : unit t

  let interrupt = Interrupt
  let continue eff = Continue eff
  let return value = Return value
  let fail ~backtrace:bt exn = Fail (bt, exn)
  let perform eff = Perform eff
  let yield = Yield
end

type perform = { perform: 'a 'b. ('a Op.t -> 'b t) -> 'a Effect.t -> 'b t }
[@@unboxed]

let once : type a. perform:perform -> a t -> a t =
 fun ~perform -> function
  | Finished _ as finished -> finished
  | Unhandled (fn, v) -> continue_with fn v
  | Suspended (fn, e) as state ->
      let k : type c. (c, a) continuation -> c Op.t -> a t =
       fun fn -> function
        | Return v -> continue_with fn v
        | Fail (bt, exn) -> discontinue_with ~backtrace:bt fn exn
        | Interrupt -> state
        | Continue e -> suspended_with fn e
        | Perform eff ->
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
  let k : type c. (c, a) continuation -> c Op.t -> a t =
   fun fn -> function
    | Return v -> continue_with fn v
    | Fail (bt, exn) -> discontinue_with ~backtrace:bt fn exn
    | Continue e -> suspended_with fn e
    | Perform e ->
        let v = Effect.perform e in
        unhandled_with fn v
    | Interrupt -> raise_notrace Break
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

let fail ~backtrace:bt ~exn = function
  | Finished _ -> Finished (Error (bt, exn))
  | Suspended (k, _) -> discontinue_with ~backtrace:bt k exn
  | Unhandled (k, _) -> discontinue_with ~backtrace:bt k exn
