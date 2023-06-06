external reraise : exn -> 'a = "%reraise"

module Promise = struct
  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  type kind = Task | Domain | Sleep of int64
  type 'a t = { uid : Uid.t; mutable state : 'a state; kind : kind }
  type 'a fn = unit -> 'a
  type _ Effect.t += Spawn : 'a t * 'a fn -> 'a t Effect.t

  let call_cc fn =
    let uid = Uid.concurrent () in
    let task = { uid; state = Pending; kind = Task } in
    Effect.perform (Spawn (task, fn))

  let call fn =
    let uid = Uid.parallel () in
    let task = { uid; state = Pending; kind = Domain } in
    Effect.perform (Spawn (task, fn))

  type _ Effect.t += Sleep : unit t -> unit t Effect.t

  let sleep ns =
    let uid = Uid.concurrent () in
    let task = { uid; state = Pending; kind = Sleep ns } in
    Effect.perform (Sleep task)

  let state { state; _ } = state

  type _ Effect.t += Await : 'a t -> ('a, exn) result Effect.t

  let await promise =
    try Effect.perform (Await promise)
    with Effect.Unhandled effect -> raise Base.(Outside (Effect effect))

  let await_exn promise =
    match await promise with Ok value -> value | Error exn -> reraise exn
end

type process =
  | Fiber : 'a Promise.t * (unit -> 'a) -> process
  | Domain : 'a Promise.t * 'a Domain.t -> process

type scheduler = { todo : process Rlist.t; current : Uid.t Atomic.t }

let next dom =
  Format.eprintf ">>> Execute a new promise.\n%!";
  match Rlist.take dom.todo with
  | Fiber (task, fn) ->
      Format.eprintf ">>> Execute a new fiber.\n%!";
      let parent = Atomic.get dom.current in
      Atomic.set dom.current task.Promise.uid;
      let value = fn () in
      task.Promise.state <- Resolved value;
      Atomic.set dom.current parent
  | Domain (_task, _domain) -> assert false
  | exception Rlist.Empty ->
      Format.eprintf ">>> The TODO list is empty.\n%!";
      ()

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

let rec await dom task k =
  match task.Promise.state with
  | Resolved value -> Effect.Deep.continue k (Ok value)
  | Failed exn -> Effect.Deep.continue k (Error exn)
  | Pending ->
      next dom;
      await dom task k

let run ?g fn =
  let go : scheduler -> (unit -> 'a) -> 'a =
   fun dom0 fn ->
    let retc value =
      Format.eprintf ">>> All call{,_cc} collected!\n%!";
      next dom0;
      value
    in
    let exnc = reraise in
    let effc :
        type c. c Effect.t -> ((c, 'a) Effect.Deep.continuation -> 'a) option =
      function
      | Uid.Uid -> Some (get_uid dom0)
      | Promise.Spawn (task, fn) -> Some (spawn dom0 task fn)
      | Promise.Await task -> Some (await dom0 task)
      | _effect -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  Uid.reset ();
  let uid = Uid.parallel () in
  let g = Option.value ~default:(Random.State.make_self_init ()) g in
  let scheduler = { todo = Rlist.make g; current = Atomic.make uid } in
  go scheduler fn
