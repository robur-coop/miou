type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation
type error = exn * Printexc.raw_backtrace

type 'a t =
  | Finished of ('a, error) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

let pp ppf = function
  | Finished (Ok _) -> Fmt.string ppf "<resolved>"
  | Finished (Error _) -> Fmt.string ppf "<errored>"
  | Suspended _ -> Fmt.string ppf "<suspended>"
  | Unhandled _ -> Fmt.string ppf "<unhandled>"

let retc value = Finished (Ok value)

let exnc exn =
  let bt = Printexc.get_raw_backtrace () in
  Finished (Error (exn, bt))

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

let handler_discontinue exn_and_bt =
  let open Effect.Shallow in
  let const _ = Finished (Error exn_and_bt) in
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
    (handler_discontinue (exn, bt))

let suspended_with : ('c, 'a) continuation -> 'c Effect.t -> 'a t =
 fun k e -> Suspended (k, e)

let unhandled_with : ('c, 'a) continuation -> 'c -> 'a t =
 fun k v -> Unhandled (k, v)

let pure res = Finished res

let make k v =
  let k = Effect.Shallow.fiber k in
  continue_with k v

module Operation = struct
  type 'a t =
    | Return of 'a
    | Fail of exn * Printexc.raw_backtrace
    | Interrupt
    | Continue : 'a Effect.t -> 'a t
    | Perform : 'a Effect.t -> 'a t
    | Yield : unit t

  let interrupt = Interrupt
  let continue eff = Continue eff
  let return value = Return value
  let fail ~backtrace:bt exn = Fail (exn, bt)
  let perform eff = Perform eff
  let yield = Yield
end

type ('a, 'b) handler = ('a Operation.t -> 'b t) -> 'a Effect.t -> 'b t
type perform = { perform: 'a 'b. ('a, 'b) handler } [@@unboxed]

let once : type a. perform:perform -> a t -> a t =
 fun ~perform -> function
  | Finished _ as finished -> finished
  | Unhandled (fn, v) -> continue_with fn v
  | Suspended (fn, e) as state ->
      let k : type c. (c, a) continuation -> c Operation.t -> a t =
       fun fn -> function
        | Return v -> continue_with fn v
        | Fail (exn, bt) -> discontinue_with ~backtrace:bt fn exn
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
  let k : type c. (c, a) continuation -> c Operation.t -> a t =
   fun fn -> function
    | Return v -> continue_with fn v
    | Fail (exn, bt) -> discontinue_with ~backtrace:bt fn exn
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
  | exn ->
      Logs.err (fun m -> m "Unexpected exception: %S" (Printexc.to_string exn));
      raise exn

[@@@warning "+8"]

let fail ~backtrace:bt ~exn = function
  | Finished _ -> Finished (Error (exn, bt))
  | Unhandled (k, _) -> begin
      try discontinue_with ~backtrace:bt k exn
      with Stdlib.Effect.Continuation_already_resumed ->
        Finished (Error (exn, bt))
    end
  | Suspended (k, _) -> begin
      try discontinue_with ~backtrace:bt k exn
      with Stdlib.Effect.Continuation_already_resumed ->
        Finished (Error (exn, bt))
    end
