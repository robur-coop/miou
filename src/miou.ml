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

exception Not_a_child
exception Still_has_children

external reraise : exn -> 'a = "%reraise"

module Prm = struct
  exception Cancelled

  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Consumed of ('a, exn) result
        (** When the parent of the task consumed the termination. *)
    | Failed of exn  (** Abnormal termination. *)

  (* NOTE(dinosaure): the state machine is:
     1) A [Pending] promise can be set to [Resolved], [Failed] or [Consumed].
        For the [Consumed] update, it can be done only by the user via [await]
        or [cancel]. [Resolved] or [Failed] can be set by the promise itself.
     2) A [Resolved] promise can be set to [Consumed] only by [await] or
        [cancel]
     3) A [Failed] promise can be set to [Consumed] only by [await] or [cancel]

     The [Consumed] state prove that the user is aware about the state of the
     promise because only [await] or [cancel] is able to set the state to
     [Consumed]. By this way, we can check that the user _consumed_ all children
     of the current task and we can safely stop the current task then.
  *)

  let to_result_exn = function
    | Pending -> invalid_arg "Prm.to_result_exn"
    | Resolved v -> Ok v
    | Failed exn -> Error exn
    | Consumed res -> res

  type kind = Task | Domain | Var

  type 'a promise = {
      domain: Uid.t
    ; state: 'a state Atomic.t
    ; kind: kind
    ; children: w Tq.t
    ; uid: Id.t
  }

  and w = Prm : 'a promise -> w
  and +!'a t

  let to_public : 'a promise -> 'a t = Obj.magic
  let of_public : 'a t -> 'a promise = Obj.magic

  type 'a fn = unit -> 'a
  type _ Effect.t += Spawn : 'a promise * 'a fn -> 'a t Effect.t

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

  let state { state; _ } = Atomic.get state
  let state prm = state (of_public prm)

  let rec failed_with : type a. a promise -> exn -> unit =
   fun { state; children; _ } exn ->
    Tq.iter ~f:(fun (Prm p) -> failed_with p Cancelled) children;
    let was_pending =
      Atomic.compare_and_set state Pending (Consumed (Error exn))
    in
    (* NOTE(dinosaure): in some cases, we want to [cancel] a promise which is
       already resolved. In this situation, nobody will really await this
       cancelled task and we will trigger the [Still_has_children]. In the case
       where the task was not pending, we must set the [state] to [Consumed] and
       assume that even if nobody consumed the value, the asked cancellation
       _consumed_ it. *)
    if not was_pending then
      match Atomic.get state with
      | Resolved value -> Atomic.set state (Consumed (Ok value))
      | Failed exn -> Atomic.set state (Consumed (Error exn))
      | _ -> ()

  let failed_with prm = failed_with (of_public prm)
  let cancel prm = failed_with prm Cancelled

  (* [is_terminated] means that the task finished with [Resolved] **or**
     [Failed]. *)
  let is_terminated { state; _ } =
    match Atomic.get state with
    | Resolved _ | Failed _ | Consumed _ -> true
    | Pending -> false

  let is_terminated prm = is_terminated (of_public prm)

  let is_consumed { state; _ } =
    match Atomic.get state with Consumed _ -> true | _ -> false

  let is_pending { state; _ } =
    match Atomic.get state with Pending -> true | _ -> false

  let is_pending prm = is_pending (of_public prm)

  type _ Effect.t += Yield : unit Effect.t
  type _ Effect.t += Await : 'a promise -> ('a, exn) result Effect.t

  let yield () = Effect.perform Yield
  let await prm = Effect.perform (Await prm)
  let await prm = await (of_public prm)

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

  let await_one = function
    | [] -> invalid_arg "Prm.await_one"
    | prms ->
        let rec go pending = function
          | [] -> go [] prms
          | prm :: _ when is_terminated prm -> await prm
          | prm :: rest ->
              yield ();
              go (prm :: pending) rest
        in
        go [] prms

  let await_one_exn lst =
    match await_one lst with Ok v -> v | Error exn -> raise exn

  let await_all_exn = function
    | [] -> invalid_arg "Prm.await_all_ign_exn"
    | prms ->
        let rec go ?exn pending prms =
          match (pending, prms) with
          | [], [] -> ( match exn with Some exn -> Error exn | None -> Ok ())
          | pending, [] -> go ?exn [] pending
          | pending, prm :: rest -> (
              match (Atomic.get (of_public prm).state, exn) with
              | Pending, _ ->
                  yield ();
                  go ?exn (prm :: pending) rest
              | (Resolved _ | Consumed (Ok _)), _ ->
                  ignore (await prm);
                  go ?exn pending rest
              | (Failed exn | Consumed (Error exn)), None ->
                  ignore (await prm);
                  go ~exn pending rest
              | Failed _, Some _ | Consumed (Error _), Some _ ->
                  ignore (await prm);
                  go ?exn pending rest)
        in
        go [] prms

  let await_all_ign lst =
    match await_all_exn lst with Ok () -> () | Error exn -> raise exn
end

module Var = struct
  type 'a var = 'a Prm.promise
  type -!'a t

  let of_public : 'a t -> 'a var = Obj.magic
  let to_public : 'a var -> 'a t = Obj.magic

  let resolve { Prm.state; _ } value =
    ignore (Atomic.compare_and_set state Prm.Pending (Prm.Resolved value));
    Prm.yield ()

  let resolve var value = resolve (of_public var) value

  type _ Effect.t += Var : 'a Prm.promise -> ('a Prm.t * 'a t) Effect.t

  let make () =
    let domain = Uid.concurrent () in
    let uid = Id.gen () in
    let promise =
      {
        Prm.uid
      ; domain
      ; state= Atomic.make Prm.Pending
      ; kind= Prm.Var
      ; children= Tq.make ()
      }
    in
    Effect.perform (Var promise)
end

type process =
  | Fiber : 'a Prm.promise * (unit -> 'a) -> process
  | Domain : scheduler * 'a Prm.promise * ('a, exn) result Domain.t -> process

and scheduler = {
    g: Random.State.t
  ; todo: process Rlist.t
  ; domain: Uid.t
  ; events: unit -> unit option
}

type go = {
    go: 'a. scheduler -> 'a Prm.promise -> (unit -> 'a) -> ('a, exn) result
}
[@@unboxed]

let rec runner dom go =
  match Rlist.take dom.todo with
  | Fiber (prm, fn) when Prm.is_pending (Prm.to_public prm) -> (
      let g = Random.State.copy dom.g in
      let dom' =
        { g; todo= Rlist.make g; domain= prm.domain; events= dom.events }
      in
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
          ignore (Atomic.compare_and_set prm.Prm.state Pending (Failed exn))
      | exception ((Not_a_child | Still_has_children) as exn) ->
          ignore
            (Atomic.compare_and_set prm.Prm.state Pending (Failed Prm.Cancelled));
          reraise exn)
  | Domain (_, prm, _) as task when Prm.is_pending (Prm.to_public prm) ->
      Rlist.push task dom.todo
  | Fiber _ | Domain _ | (exception Rlist.Empty) ->
      let event = dom.events () in
      if Option.is_some event then
        runner dom go
      else
        Domain.cpu_relax ()

let get_uid dom k = Effect.Deep.continue k dom.domain

let spawn ~parent dom { go } prm fn k =
  let process =
    match prm.Prm.kind with
    | Prm.Var ->
        assert false (* XXX(dinosaure): this case should **never** occur! *)
    | Prm.Task -> Fiber (prm, fn)
    | Prm.Domain ->
        let g = Random.State.copy dom.g in
        let dom' =
          { g; todo= Rlist.make g; domain= prm.Prm.domain; events= dom.events }
        in
        let value =
          Domain.spawn (fun () ->
              try go dom' prm fn
              with (Not_a_child | Still_has_children) as exn ->
                ignore
                  (Atomic.compare_and_set prm.Prm.state Pending (Failed exn));
                reraise exn)
        in
        (* NOTE(dinosaure): in parallel! *)
        Domain (dom', prm, value)
  in
  (* NOTE(dinosaure): add the promise into parent's [children]
     and into our [todo] list. *)
  Tq.enqueue parent.Prm.children (Prm.Prm prm);
  Rlist.push process dom.todo;
  Effect.Deep.continue k (Prm.to_public prm)

let var ~parent prm k =
  assert (prm.Prm.kind = Prm.Var);
  Tq.enqueue parent.Prm.children (Prm.Prm prm);
  Effect.Deep.continue k (Prm.to_public prm, Var.to_public prm)

(* Here, we verify that we await a children of the current [dom] and run
   [until_is_resolved] *)
let rec await ~parent dom go prm k =
  (* NOTE(dinosaure): [children] is more accurate than [dom.todo]! Even if
     [spawn] adds [prm] into [children] AND [todo], the first one is
     thread-safe. It's better to use it in our context. *)
  let res = ref false in
  Tq.iter
    ~f:(fun (Prm.Prm child) -> res := !res || child.uid = prm.Prm.uid)
    parent.Prm.children;
  if !res = false then
    Effect.Deep.discontinue k Not_a_child
  else
    until_is_resolved dom go prm k

and until_is_resolved dom go prm k =
  (* NOTE(dinosaure): we do this loop until [prm] is resolved regardless the
     [prm]'s kind. Indeed, even for a [Domain], we set [prm.state] at the final
     stage (see [retc]). *)
  match Atomic.get prm.state with
  | Consumed res -> Effect.Deep.continue k res
  | Resolved v as res ->
      ignore (Atomic.compare_and_set prm.state res (Consumed (Ok v)));
      Effect.Deep.continue k (Ok v)
  | Failed exn as res ->
      ignore (Atomic.compare_and_set prm.state res (Consumed (Error exn)));
      Effect.Deep.continue k (Error exn)
  | Pending ->
      runner dom go;
      until_is_resolved dom go prm k

let collect_pending_promises dom =
  let rec go pending =
    match Rlist.take dom.todo with
    | Fiber (prm, _) when Prm.is_consumed prm -> go pending
    | Domain (_, prm, _) when Prm.is_consumed prm ->
        (* TODO(dinosaure): we probably should [Domain.join] to be sure that the
           resource is released. However, it seems that when we cancel a domain
           child from its parent, it never enters into the [Failed] case (it
           does not have the opportunity). The basic solution will be to
           implement our [Unix.sleep] function which will call [runner]
           sometimes and give an opportunity to the domain to see that it was
           already [Cancelled]. *)
        go pending
    | Fiber (prm, _) -> go (Prm.Prm prm :: pending)
    | Domain (_, prm, _) -> go (Prm.Prm prm :: pending)
    | exception Rlist.Empty -> pending
  in
  go []

let collect_pending_children prm =
  let rec go pending =
    match Tq.dequeue prm.Prm.children with
    | Prm.Prm prm when Prm.is_consumed prm -> go pending
    | Prm.Prm prm -> go (Prm.Prm prm :: pending)
    | exception Tq.Empty -> pending
  in
  go []

let run ?g ?(events = Fun.const None) fn =
  let rec go :
      type a. scheduler -> a Prm.promise -> (unit -> a) -> (a, exn) result =
   fun dom0 cur fn ->
    let retc value =
      (* NOTE(dinosaure): here, we set the current promise to [Resolved] but
         someone else (the parent) must consume (via [await] or [cancel]) the
         value. *)
      let _ = Atomic.compare_and_set cur.Prm.state Pending (Resolved value) in
      match collect_pending_children cur with
      | [] ->
          (* NOTE(dinosaure): it's safe to use [to_result_exn]. At least,
             [cur.Prm.state] was set to [Resolved _]. *)
          Prm.to_result_exn (Atomic.get cur.Prm.state)
      | _ -> raise Still_has_children
    in
    let exnc = function
      | (Not_a_child | Still_has_children) as exn -> reraise exn
      | exn ->
          (* NOTE(dinosaure): here, we must set the promise to [Failed] and
             expect that the user [cancel]led or [await]ed the task. *)
          Atomic.set cur.Prm.state (Failed exn);
          Error exn
    in
    let effc :
        type c a. c Effect.t -> ((c, a) Effect.Deep.continuation -> a) option =
     fun eff ->
      (* NOTE(dinosaure): we must be preemptive on the [Failed] case. *)
      match (Atomic.get cur.state, eff) with
      | Prm.Failed exn, _ ->
          let continuation k =
            (* NOTE(dinosaure): children should be cancelled. *)
            assert (collect_pending_promises dom0 = []);
            Effect.Deep.discontinue k exn
          in
          Some continuation
      | _, Uid.Uid -> Some (get_uid dom0)
      | _, Prm.Spawn (prm, fn) -> Some (spawn ~parent:cur dom0 { go } prm fn)
      | _, Var.Var prm -> Some (var ~parent:cur prm)
      | _, Prm.Yield ->
          let continuation k =
            runner dom0 { go };
            Effect.Deep.continue k ()
          in
          Some continuation
      | _, Prm.Await prm -> Some (await ~parent:cur dom0 { go } prm)
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
  let dom0 = { g; todo= Rlist.make g; domain= uid; events } in
  go dom0 prm fn

let run ?g ?events fn =
  match run ?events ?g fn with Ok v -> v | Error exn -> raise exn
