module Tq = Tq

external reraise : exn -> 'a = "%reraise"

module Promise = struct
  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  type kind = Task | Domain | Sleep of int64
  type 'a t = { uid : Uid.t; state : 'a state Atomic.t; kind : kind }
  type 'a fn = unit -> 'a
  type _ Effect.t += Spawn : 'a t * 'a fn -> 'a t Effect.t

  let call_cc fn =
    let uid = Uid.concurrent () in
    let task = { uid; state = Atomic.make Pending; kind = Task } in
    Effect.perform (Spawn (task, fn))

  let call fn =
    let uid = Uid.parallel () in
    let task = { uid; state = Atomic.make Pending; kind = Domain } in
    Effect.perform (Spawn (task, fn))

  type _ Effect.t += Sleep : unit t -> unit t Effect.t

  let sleep ns =
    let uid = Uid.concurrent () in
    let task = { uid; state = Atomic.make Pending; kind = Sleep ns } in
    Effect.perform (Sleep task)

  exception Cancelled

  let state { state; _ } = Atomic.get state

  let cancel { state; _ } =
    (* NOTE(dinosaure): we set promise to [Failed] only if the promise is
       [Pending]. If the promise had already returned by the time cancel was
       called, **no** state transitions are made. *)
    Atomic.compare_and_set state Pending (Failed Cancelled) |> ignore

  let is_terminated { state; _ } =
    match Atomic.get state with
    | Resolved _ | Failed _ -> true
    | Pending -> false

  type _ Effect.t += Yield : unit Effect.t

  let yield () = Effect.perform Yield

  (* TODO(dinosaure): check if we await a /right/ children according to the
     curren [uid]. *)
  let rec await promise =
    match Atomic.get promise.state with
    | Resolved v ->
        (* TODO(dinosaure): check if all children are resolved. Only for domains? *)
        Ok v
    | Failed exn -> Error exn
    | Pending ->
        Effect.perform Yield;
        await promise

  let await_exn promise =
    match await promise with Ok value -> value | Error exn -> reraise exn

  let await_first = function
    | [] -> invalid_arg "Promise.await_first"
    | promises ->
        let rec go pending = function
          | [] -> go [] promises
          | promise :: rest ->
              if is_terminated promise then (
                List.iter cancel (List.rev_append pending rest);
                await promise)
              else (
                yield ();
                go (promise :: pending) rest)
        in
        go [] promises
end

type process =
  | Fiber : 'a Promise.t * (unit -> 'a) -> process
  | Domain : 'a Promise.t * 'a Domain.t -> process

type scheduler = { todo : process Rlist.t; current : Uid.t Atomic.t }

let yield dom k =
  match Rlist.take dom.todo with
  | Fiber (promise, fn) ->
      assert (Atomic.get dom.current = promise.Promise.uid);
      (* TODO(dinosaure): check that! We mention that [yield] gives other
         promises of the **same uid** a chance to run. *)
      let value = fn () in
      (* NOTE(dinosaure): here, we set the promise to [Resolved] **only if**
         nobody set it to [Failed]. [is_resolved] confirms (if it's [true]) that
         we set our promise to [Resolved]. *)
      let is_resolved =
        Atomic.compare_and_set promise.Promise.state Pending (Resolved value)
      in
      if is_resolved then Effect.Deep.continue k () else assert false
  | Domain (_task, _domain) -> assert false
  | exception Rlist.Empty -> Effect.Deep.continue k ()

let get_uid dom k =
  let uid = Atomic.get dom.current in
  Effect.Deep.continue k uid

let spawn dom task fn k =
  let process =
    match task.Promise.kind with
    | Promise.Task -> Fiber (task, fn)
    | Promise.Domain ->
        let dom' = Domain.spawn fn in
        Domain (task, dom')
    | Promise.Sleep _ -> assert false
  in
  Rlist.push process dom.todo;
  Effect.Deep.continue k task

type promise = Promise : 'a Promise.t -> promise

let collect_pending_promises dom =
  let rec go pending =
    match Rlist.take dom.todo with
    | Fiber (promise, _) when Promise.is_terminated promise -> go pending
    | Fiber (promise, _) -> go (Promise promise :: pending)
    | Domain _ -> go pending
    | exception Rlist.Empty -> pending
  in
  go []

exception Still_has_children

let run ?g fn =
  let go : scheduler -> (unit -> 'a) -> 'a =
   fun dom0 fn ->
    let retc value =
      match collect_pending_promises dom0 with
      | [] -> value
      | _ -> raise Still_has_children
    in
    let exnc = reraise in
    let effc :
        type c. c Effect.t -> ((c, 'a) Effect.Deep.continuation -> 'a) option =
      function
      | Uid.Uid -> Some (get_uid dom0)
      | Promise.Spawn (task, fn) -> Some (spawn dom0 task fn)
      | Promise.Yield -> Some (yield dom0)
      | _effect -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  Uid.reset ();
  let uid = Uid.parallel () in
  let g = Option.value ~default:(Random.State.make_self_init ()) g in
  let scheduler = { todo = Rlist.make g; current = Atomic.make uid } in
  go scheduler fn
