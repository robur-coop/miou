module Tq = struct
  include Tq

  let rec iter ~f t =
    match dequeue t with v -> f v; iter ~f t | exception Empty -> ()
end

external reraise : exn -> 'a = "%reraise"

module Prm = struct
  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  type kind = Task | Domain | Sleep of int64

  type 'a t = {
      uid: Uid.t
    ; state: 'a state Atomic.t
    ; kind: kind
    ; children: w Tq.t
  }

  and w = Prm : 'a t -> w

  type 'a fn = unit -> 'a
  type _ Effect.t += Spawn : 'a t * 'a fn -> 'a t Effect.t

  let call_cc fn =
    let uid = Uid.concurrent () in
    let task =
      { uid; state= Atomic.make Pending; kind= Task; children= Tq.make () }
    in
    Effect.perform (Spawn (task, fn))

  let call fn =
    let uid = Uid.parallel () in
    let task =
      { uid; state= Atomic.make Pending; kind= Domain; children= Tq.make () }
    in
    Effect.perform (Spawn (task, fn))

  type _ Effect.t += Sleep : unit t -> unit t Effect.t

  let sleep ns =
    let uid = Uid.concurrent () in
    let task =
      { uid; state= Atomic.make Pending; kind= Sleep ns; children= Tq.make () }
    in
    Effect.perform (Sleep task)

  exception Cancelled

  let state { state; _ } = Atomic.get state

  let rec cancel : type a. a t -> unit =
   fun { state; children; _ } ->
    Tq.iter ~f:(fun (Prm p) -> cancel p) children;
    (* NOTE(dinosaure): we set promise to [Failed] only if the promise is
       [Pending]. If the promise had already returned by the time cancel was
       called, **no** state transitions are made. *)
    Atomic.compare_and_set state Pending (Failed Cancelled) |> ignore

  let failed_with { state; children; _ } exn =
    Tq.iter ~f:(fun (Prm p) -> cancel p) children;
    Atomic.compare_and_set state Pending (Failed exn) |> ignore

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
    | Pending -> Effect.perform Yield; await promise

  let await_exn promise =
    match await promise with Ok value -> value | Error exn -> reraise exn

  let await_first = function
    | [] -> invalid_arg "Prm.await_first"
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
  | Fiber : 'a Prm.t * (unit -> 'a) -> process
  | Domain : 'a Prm.t * 'a Domain.t -> process

type scheduler = {
    g: Random.State.t
  ; todo: process Rlist.t
  ; domain: Uid.t
  ; current: Prm.w
}

type go = { go: 'a. scheduler -> (unit -> 'a) -> 'a } [@@unboxed]

let yield : scheduler -> go -> ('a, 'b) Effect.Deep.continuation -> 'b =
 fun dom go k ->
  match Rlist.take dom.todo with
  | Fiber (promise, fn) -> (
      let g = Random.State.copy dom.g in
      let dom' =
        { g; todo= Rlist.make g; domain= promise.uid; current= Prm.Prm promise }
      in
      assert (dom.domain = promise.Prm.uid);
      (* TODO(dinosaure): check that! We mention that [yield] gives other
         promises of the **same uid** a chance to run. *)
      try
        let value = go.go dom' fn in
        (* NOTE(dinosaure): here, we set the promise to [Resolved] **only if**
           nobody set it to [Failed]. [is_resolved] confirms (if it's [true]) that
           we set our promise to [Resolved]. *)
        ignore
          (Atomic.compare_and_set promise.Prm.state Pending (Resolved value));
        Effect.Deep.continue k ()
      with exn ->
        ignore (Atomic.compare_and_set promise.Prm.state Pending (Failed exn));
        Effect.Deep.continue k ())
  | Domain (_task, _domain) -> assert false
  | exception Rlist.Empty -> Effect.Deep.continue k ()

let get_uid dom k = Effect.Deep.continue k dom.domain

let spawn dom promise fn k =
  let process =
    match promise.Prm.kind with
    | Prm.Task -> Fiber (promise, fn)
    | Prm.Domain ->
        let dom' = Domain.spawn fn in
        Domain (promise, dom')
    | Prm.Sleep _ -> assert false
  in
  let (Prm.Prm parent) = dom.current in
  (* NOTE(dinosaure): add the promise into parent's [children]
     and into our [todo] list. *)
  Tq.enqueue parent.children (Prm.Prm promise);
  Rlist.push process dom.todo;
  Effect.Deep.continue k promise

let collect_pending_promises dom =
  let rec go pending =
    match Rlist.take dom.todo with
    | Fiber (promise, _) when Prm.is_terminated promise -> go pending
    | Fiber (promise, _) -> go (Prm.Prm promise :: pending)
    | Domain _ -> go pending
    | exception Rlist.Empty -> pending
  in
  go []

exception Still_has_children

let run ?g fn =
  let rec go : type a. scheduler -> (unit -> a) -> a =
   fun dom0 fn ->
    let retc value =
      match collect_pending_promises dom0 with
      | [] -> value
      | _ -> raise Still_has_children
    in
    let exnc exn =
      let (Prm promise) = dom0.current in
      Prm.failed_with promise exn;
      reraise exn
    in
    let effc :
        type c a. c Effect.t -> ((c, a) Effect.Deep.continuation -> a) option =
      function
      | Uid.Uid -> Some (get_uid dom0)
      | Prm.Spawn (task, fn) -> Some (spawn dom0 task fn)
      | Prm.Yield -> Some (yield dom0 { go })
      | _effect -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  Uid.reset ();
  let uid = Uid.parallel () in
  let g = Option.value ~default:(Random.State.make_self_init ()) g in
  let promise =
    {
      Prm.uid
    ; state= Atomic.make Prm.Pending
    ; kind= Prm.Domain
    ; children= Tq.make ()
    }
  in
  let scheduler =
    { g; todo= Rlist.make g; domain= uid; current= Prm.Prm promise }
  in
  go scheduler fn
