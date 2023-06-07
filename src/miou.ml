module Tq = Tq

module Id = struct
  type t = int

  let epsilon = -1
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int

  let gen =
    let value = Atomic.make (epsilon + 1) in
    fun () -> Atomic.fetch_and_add value 1
end

external reraise : exn -> 'a = "%reraise"

module Prm = struct
  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  type kind = Task | Domain

  type 'a t = {
      domain: Uid.t
    ; state: 'a state Atomic.t
    ; kind: kind
    ; children: w Tq.t
    ; uid: Id.t
  }

  and w = Prm : 'a t -> w

  type 'a fn = unit -> 'a
  type _ Effect.t += Spawn : 'a t * 'a fn -> 'a t Effect.t

  let call_cc fn =
    let domain = Uid.concurrent () in
    let uid = Id.gen () in
    let promise =
      {
        uid
      ; domain
      ; state= Atomic.make Pending
      ; kind= Task
      ; children= Tq.make ()
      }
    in
    Effect.perform (Spawn (promise, fn))

  let call fn =
    let domain = Uid.parallel () in
    let uid = Id.gen () in
    let promise =
      {
        uid
      ; domain
      ; state= Atomic.make Pending
      ; kind= Domain
      ; children= Tq.make ()
      }
    in
    Effect.perform (Spawn (promise, fn))

  exception Cancelled

  let state { state; _ } = Atomic.get state

  let rec failed_with : type a. a t -> exn -> unit =
   fun { state; children; _ } exn ->
    Tq.iter ~f:(fun (Prm p) -> failed_with p Cancelled) children;
    Atomic.compare_and_set state Pending (Failed exn) |> ignore

  let cancel prm = failed_with prm Cancelled

  (* [is_terminated] means that the task finished with [Resolved] **or**
     [Failed]. *)
  let is_terminated { state; _ } =
    match Atomic.get state with
    | Resolved _ | Failed _ -> true
    | Pending -> false

  type _ Effect.t += Yield : unit Effect.t
  type _ Effect.t += Await : 'a t -> ('a, exn) result Effect.t

  let yield () = Effect.perform Yield
  let await prm = Effect.perform (Await prm)

  let await_exn prm =
    match await prm with Ok value -> value | Error exn -> reraise exn

  let await_first = function
    | [] -> invalid_arg "Prm.await_first"
    | prms ->
        let rec go pending = function
          | [] -> go [] prms
          | prm :: rest when is_terminated prm ->
              (* TODO(dinosaure): we probably should aggregate resolved promises
                 and randomly take one! *)
              List.iter cancel (List.rev_append pending rest);
              await prm
          | prm :: rest ->
              yield ();
              go (prm :: pending) rest
        in
        go [] prms

  let await_first_exn lst =
    match await_first lst with Ok v -> v | Error exn -> raise exn
end

type process =
  | Fiber : 'a Prm.t * (unit -> 'a) -> process
  | Domain : scheduler * 'a Prm.t * ('a, exn) result Domain.t -> process

and scheduler = { g: Random.State.t; todo: process Rlist.t; domain: Uid.t }

type go = { go: 'a. scheduler -> 'a Prm.t -> (unit -> 'a) -> ('a, exn) result }
[@@unboxed]

let runner dom go =
  match Rlist.take dom.todo with
  | Fiber (prm, fn) -> (
      let g = Random.State.copy dom.g in
      let dom' = { g; todo= Rlist.make g; domain= prm.domain } in
      assert (dom.domain = prm.Prm.domain);
      (* TODO(dinosaure): check that! We mention that [yield] gives other
         promises of the **same uid** a chance to run. *)
      match go.go dom' prm fn with
      | Ok value ->
          (* NOTE(dinosaure): here, we set the promise to [Resolved] **only if**
             nobody set it to [Failed]. [is_resolved] confirms (if it's [true]) that
             we set our promise to [Resolved]. *)
          ignore (Atomic.compare_and_set prm.Prm.state Pending (Resolved value))
      | Error exn ->
          ignore (Atomic.compare_and_set prm.Prm.state Pending (Failed exn)))
  | Domain _ as task -> Rlist.push task dom.todo
  | exception Rlist.Empty -> ()

let get_uid dom k = Effect.Deep.continue k dom.domain

let spawn ~parent dom { go } prm fn k =
  let process =
    match prm.Prm.kind with
    | Prm.Task -> Fiber (prm, fn)
    | Prm.Domain ->
        let g = Random.State.copy dom.g in
        let dom' = { g; todo= Rlist.make g; domain= prm.Prm.domain } in
        let value = Domain.spawn (fun () -> go dom' prm fn) in
        (* NOTE(dinosaure): in parallel! *)
        Domain (dom', prm, value)
  in
  (* NOTE(dinosaure): add the promise into parent's [children]
     and into our [todo] list. *)
  Tq.enqueue parent.Prm.children (Prm.Prm prm);
  Rlist.push process dom.todo;
  Effect.Deep.continue k prm

exception Not_a_child

(* Here, we verify that we await a children of the current [dom] and run
   [until_is_resolved] *)
let rec await ~parent dom go prm k =
  (* NOTE(dinosaure): [children] is more accurate than [dom.todo]! Even if [spawn]
     adds [prm] into [children] AND [todo], the first one is thread-safe. It's better
     to use it in our context. *)
  let res = ref false in
  Tq.iter
    ~f:(fun (Prm.Prm child) -> res := !res || child.uid = prm.Prm.uid)
    parent.Prm.children;
  if !res = false then
    Effect.Deep.discontinue k Not_a_child
  else
    until_is_resolved dom go prm k

and until_is_resolved dom go prm k =
  match Atomic.get prm.state with
  | Resolved v -> Effect.Deep.continue k (Ok v)
  | Failed exn -> Effect.Deep.continue k (Error exn)
  | Pending ->
      runner dom go;
      until_is_resolved dom go prm k

let collect_pending_promises dom =
  let rec go pending =
    match Rlist.take dom.todo with
    | Fiber (prm, _) when Prm.is_terminated prm -> go pending
    | Domain (_, prm, _) when Prm.is_terminated prm -> go pending
    | Fiber (prm, _) -> go (Prm.Prm prm :: pending)
    | Domain (_, prm, _) -> go (Prm.Prm prm :: pending)
    | exception Rlist.Empty -> pending
  in
  go []

exception Still_has_children

let run ?g fn =
  let rec go : type a. scheduler -> a Prm.t -> (unit -> a) -> (a, exn) result =
   fun dom0 current fn ->
    let retc value =
      let _ =
        Atomic.compare_and_set current.Prm.state Pending (Resolved value)
      in
      match collect_pending_promises dom0 with
      | [] -> Ok value
      | _ -> raise Still_has_children
    in
    let exnc = function
      | (Not_a_child | Still_has_children) as exn -> reraise exn
      | exn ->
          Prm.failed_with current exn;
          Error exn
    in
    let effc :
        type c a. c Effect.t -> ((c, a) Effect.Deep.continuation -> a) option =
     fun eff ->
      (* NOTE(dinosaure): we must be preemptive on the [Failed] case. *)
      match (Atomic.get current.state, eff) with
      | Prm.Failed exn, _ ->
          let continuation k =
            assert (collect_pending_promises dom0 = []);
            Effect.Deep.discontinue k exn
          in
          Some continuation
      | _, Uid.Uid -> Some (get_uid dom0)
      | _, Prm.Spawn (prm, fn) ->
          Some (spawn ~parent:current dom0 { go } prm fn)
      | _, Prm.Yield ->
          let continuation k =
            runner dom0 { go };
            Effect.Deep.continue k ()
          in
          Some continuation
      | _, Prm.Await prm -> Some (await ~parent:current dom0 { go } prm)
      | _ -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  Uid.reset ();
  let uid = Uid.parallel () in
  let g = Option.value ~default:(Random.State.make_self_init ()) g in
  let prm =
    {
      Prm.uid= Id.gen ()
    ; domain= Uid.parallel ()
    ; state= Atomic.make Prm.Pending
    ; kind= Prm.Domain
    ; children= Tq.make ()
    }
  in
  let dom0 = { g; todo= Rlist.make g; domain= uid } in
  go dom0 prm fn

let run ?g fn = match run ?g fn with Ok v -> v | Error exn -> raise exn
