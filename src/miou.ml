module Tq = Tq

module Id = struct
  type t = int

  let null = -1
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int

  let gen =
    let value = Atomic.make (null + 1) in
    fun () -> Atomic.fetch_and_add value 1
end

exception Not_a_child
exception Still_has_children

external reraise : exn -> 'a = "%reraise"

module Prm = struct
  exception Cancelled

  type 'a private_state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Consumed of ('a, exn) result
        (** When the parent of the task consumed the termination. *)
    | Failed of exn  (** Abnormal termination. *)

  (* NOTE(dinosaure): Usually, we should only have 3 states:
     - Pending
     - Resolved
     - Failed

     However, we want to check that the user _consumed_ the result of the
     promise. The rule is: in this implementation nobody can set the state
     to [Consumed] except [await] and [cancel]. The [Consumed] is of interest
     only to us. We provide a _public_ [state] and a _private_ [internal_state].
     The public one will only informs our usual states. *)

  let to_result_exn = function
    | Pending -> invalid_arg "Prm.to_result_exn"
    | Resolved v -> Ok v
    | Failed exn -> Error exn
    | Consumed res -> res

  type kind = Task | Domain | Syscall
  type resource = Resource : 'a * ('a -> unit) -> resource [@@warning "-37"]

  type 'a promise = {
      domain: Uid.t
    ; state: 'a private_state Atomic.t
    ; kind: kind
    ; children: w Tq.t
    ; uid: Id.t
    ; resources: resource Tq.t
  }

  and w = Prm : 'a promise -> w
  and +!'a t

  (* NOTE(dinosaure): this is a bit ugly but we must expose [+!'a] to the user
     and provide, by this way, a type-safe usafe of promises and [Sysc.t]. *)
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
      ; resources= Tq.make ()
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
      ; resources= Tq.make ()
      }
    in
    Effect.perform (Spawn (promise, fn))

  let rec failed_with : type a. a promise -> exn -> unit =
   fun { state; children; _ } exn ->
    Tq.iter ~f:(fun (Prm p) -> cancelled p) children;

    (* NOTE(dinosaure): in some cases, we want to [cancel] a promise which is
       already resolved. In this situation, the user will **not** really await
       this cancelled task and we will trigger the [Still_has_children]. In the
       case where the task was not pending and we want to cancel it, we must set
       the [state] to [Consumed] and assume that even if nobody consumed the
       value, the asked cancellation _consumed_ it in a certain way.

       There is, however, one subtlety concerning children. The cancellation
       must be propagated, but without confirming that they have been
       [Consumed] by someone. [cancelled] does the propagation with [Failed]
       instead of [Consumed]. *)
    if not (Atomic.compare_and_set state Pending (Consumed (Error exn))) then
      match Atomic.get state with
      | Resolved value -> Atomic.set state (Consumed (Ok value))
      | Failed exn -> Atomic.set state (Consumed (Error exn))
      | _ -> ()

  and cancelled : type a. a promise -> unit =
   fun { state; children; _ } ->
    Tq.iter ~f:(fun (Prm p) -> cancelled p) children;
    ignore (Atomic.compare_and_set state Pending (Failed Cancelled))

  let failed_with prm = failed_with (of_public prm)
  let cancel prm = failed_with prm Cancelled

  (* [is_terminated] means that the task finished with [Resolved] **or**
     [Failed]. This function is **not** exposed. *)
  let is_terminated { state; _ } =
    match Atomic.get state with
    | Resolved _ | Failed _ | Consumed _ -> true
    | Pending -> false

  let is_resolved { state; _ } =
    match Atomic.get state with
    | Resolved _ | Consumed (Ok _) -> true
    | _ -> false

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

  type _ Effect.t += Await_first : 'a promise list -> ('a, exn) result Effect.t

  let await_first = function
    | [] -> invalid_arg "Prm.await_first"
    | prms -> Effect.perform (Await_first (List.map of_public prms))

  let await_first_exn lst =
    match await_first lst with Ok v -> v | Error exn -> raise exn

  (* TODO(dinosaure): perform an effect. *)
  let await_one = function
    | [] -> invalid_arg "Prm.await_one"
    | prms ->
        let rec go pending = function
          | [] -> go [] pending
          | prm :: _ when is_terminated (of_public prm) -> await prm
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

  type 'a state = Pending | Resolved of 'a | Failed of exn

  let state { state; _ } =
    match Atomic.get state with
    | Pending -> Pending
    | Resolved v | Consumed (Ok v) -> Resolved v
    | Failed exn | Consumed (Error exn) -> Failed exn

  let state prm = state (of_public prm)
end

module Sysc = struct
  type 'a syscall = 'a Prm.promise
  type -!'a t

  let of_public : 'a t -> 'a syscall = Obj.magic
  let to_public : 'a syscall -> 'a t = Obj.magic

  let make () =
    let domain = Uid.concurrent () in
    let uid = Id.gen () in
    let promise =
      {
        Prm.uid
      ; domain
      ; state= Atomic.make (Prm.Pending : _ Prm.private_state)
      ; kind= Prm.Syscall
      ; children= Tq.make ()
      ; resources= Tq.make ()
      }
    in
    to_public promise

  type _ Effect.t += Syscall : 'a Prm.promise -> ('a, exn) result Effect.t

  let await prm = Effect.perform (Syscall (of_public prm))
  let uid { Prm.uid; _ } = uid
  let uid syscall = uid (of_public syscall)
end

type syscall = Syscall : 'a Sysc.t * (unit -> 'a) -> syscall

type process =
  | Fiber : 'a Prm.promise * (unit -> 'a) -> process
  | Domain : dom * 'a Prm.promise * ('a, exn) result Domain.t -> process

and dom = {
    g: Random.State.t
  ; todo: process Rlist.t
  ; domain: Uid.t
  ; events: unit -> syscall list option
  ; wait: Mutex.t * Condition.t
}

type go = { go: 'a. dom -> 'a Prm.promise -> (unit -> 'a) -> ('a, exn) result }
[@@unboxed]

let runner dom go prm =
  if Atomic.get prm.Prm.state = Pending then
    match Rlist.take dom.todo with
    | Fiber (prm, fn) when Prm.is_pending (Prm.to_public prm) -> (
        let g = Random.State.copy dom.g in
        let dom' =
          {
            g
          ; todo= Rlist.make g
          ; domain= prm.domain
          ; events= dom.events
          ; wait= dom.wait
          }
        in
        assert (dom.domain = prm.Prm.domain);
        (* TODO(dinosaure): check that! We mention that [yield] gives other
           promises of the **same uid** a chance to run. *)
        match go.go dom' prm fn with
        | Ok value ->
            (* NOTE(dinosaure): here, we set the promise to [Resolved] **only if**
               nobody set it to [Failed]. [is_resolved] confirms (if it's [true])
               that we set our promise to [Resolved]. *)
            ignore
              (Atomic.compare_and_set prm.Prm.state Pending (Resolved value))
        | Error exn ->
            ignore (Atomic.compare_and_set prm.Prm.state Pending (Failed exn))
        | exception ((Not_a_child | Still_has_children) as exn) ->
            ignore
              (Atomic.compare_and_set prm.Prm.state Pending
                 (Failed Prm.Cancelled));
            reraise exn)
    | Domain (_, prm, _) as process when Prm.is_pending (Prm.to_public prm) ->
        Domain.cpu_relax ();
        Rlist.push process dom.todo
    | Domain (_, prm, domain) when Prm.is_resolved prm -> (
        (* TODO(dinosaure): [Domain.join] can raise an exception, should we
           reraise them? *)
        try ignore (Domain.join domain) with _ -> ())
    | Fiber _ | Domain _ | (exception Rlist.Empty) -> (
        match dom.events () with
        | Some tasks ->
            List.iter
              (fun (Syscall (prm, fn)) ->
                Rlist.push (Fiber (Sysc.of_public prm, fn)) dom.todo)
              tasks
        | None -> Domain.cpu_relax ())

let get_uid dom k = Effect.Deep.continue k dom.domain

let spawn ~parent dom { go } prm fn k =
  let process =
    match prm.Prm.kind with
    | Prm.Syscall ->
        assert false (* XXX(dinosaure): this case should **never** occur! *)
    | Prm.Task -> Fiber (prm, fn)
    | Prm.Domain ->
        let g = Random.State.copy dom.g in
        let c = snd dom.wait in
        let dom' =
          {
            g
          ; todo= Rlist.make g
          ; domain= prm.Prm.domain
          ; events= dom.events
          ; wait= (Mutex.create (), Condition.create ())
          }
        in
        let value =
          Domain.spawn (fun () ->
              try
                let v = go dom' prm fn in
                Condition.signal c; v
              with (Not_a_child | Still_has_children) as exn ->
                ignore
                  (Atomic.compare_and_set prm.Prm.state Pending (Failed exn));
                Condition.signal c;
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

let all_tasks_are_domains dom =
  let res = ref (not (Rlist.is_empty dom.todo)) in
  Rlist.iter
    ~f:(function
      | Domain (_, prm, _) -> res := !res && Prm.is_pending (Prm.to_public prm)
      | _ -> res := false)
    dom.todo;
  !res

let rec until_is_resolved dom go (prm : 'a Prm.promise) k =
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
      (* NOTE(dinosaure): here, we give a chance to do another task which
         can probably set our current [prm] to [Resolved]. *)
      runner dom go prm;
      if all_tasks_are_domains dom then (
        let m, c = dom.wait in
        Mutex.lock m; Condition.wait c m; Mutex.unlock m);
      until_is_resolved dom go prm k

(* NOTE(dinosaure): here, we verify that we await a children of the current
   [parent] and run [until_is_resolved]. The [dom] contains a list of tasks (see
   [dom.todo]) which should only resolve parent's children. So we must check
   that we want to [await] a true child because we only have the ability to run
   tasks from [dom.todo]. *)
let await ~parent dom go prm k =
  let res = ref false in
  Tq.iter
    ~f:(fun (Prm.Prm child) -> res := !res || child.uid = prm.Prm.uid)
    parent.Prm.children;
  if !res = false then
    Effect.Deep.discontinue k Not_a_child
  else
    until_is_resolved dom go prm k

let await_first ~parent dom go prms k =
  let f prm =
    let res = ref false in
    Tq.iter
      ~f:(fun (Prm.Prm child) -> res := !res || child.uid = prm.Prm.uid)
      parent.Prm.children;
    !res
  in
  if not (List.for_all f prms) then
    Effect.Deep.discontinue k Not_a_child
  else
    let rec wait prms =
      let resolved, unresolved = List.partition Prm.is_terminated prms in
      match resolved with
      | [] ->
          let len = List.length unresolved in
          let prm = List.nth unresolved (Random.State.int dom.g len) in
          (* NOTE(dinosaure): that's a special case here! If we only have
             domains, that mostly means that we busy-wait that states are
             updated. We put a [condition] into all [dom]s and children
             (as domains) should notify via this condition that they finished
             their job (all children). By this way, we replace the busy-wait
             loop by an interrupt-driven process. *)
          if all_tasks_are_domains dom then (
            let m, c = dom.wait in
            Mutex.lock m; Condition.wait c m; Mutex.unlock m);
          runner dom go prm;
          wait prms
      | _ :: _ ->
          let len = List.length resolved in
          let prm = List.nth resolved (Random.State.int dom.g len) in
          List.iter (fun prm -> Prm.cancel (Prm.to_public prm)) unresolved;
          until_is_resolved dom go prm k
    in
    wait prms

let syscall ~parent dom go prm k =
  assert (prm.Prm.kind = Prm.Syscall);
  Tq.enqueue parent.Prm.children (Prm.Prm prm);
  await ~parent dom go prm k

let collect_pending_children prm =
  let rec go pending =
    match Tq.dequeue prm.Prm.children with
    | Prm.Prm prm when Prm.is_consumed prm -> go pending
    | Prm.Prm prm -> go (Prm.Prm prm :: pending)
    | exception Tq.Empty -> pending
  in
  go []

(* NOTE(dinosaure): we must [Domain.join] only promises which are resolved. In
   the case of a cancelled domain, it is probably not aware of its state.
   It probably still continues to do something and it is _unjoined_. Even the
   [Consumed] state lie to us because the cancellation _consumes_ the promise
   (if a parent cancels a child, we set the state to [Consumed (Error Failed)]).
*)
let join_resolved_domains dom =
  let rec go () =
    match Rlist.take dom.todo with
    | Domain (_, prm, domain) when Prm.is_resolved prm ->
        ignore (Domain.join domain);
        go ()
    | _ -> go ()
    | exception Rlist.Empty -> ()
  in
  go ()

let run ?g ?(events = Fun.const None) fn =
  let rec go : type a. dom -> a Prm.promise -> (unit -> a) -> (a, exn) result =
   fun dom0 cur fn ->
    let retc value =
      (* NOTE(dinosaure): here, we set the current promise to [Resolved] but
         someone else (the parent) must consume (via [await] or [cancel]) the
         value. *)
      let _ = Atomic.compare_and_set cur.Prm.state Pending (Resolved value) in
      match collect_pending_children cur with
      | [] ->
          join_resolved_domains dom0;
          (* NOTE(dinosaure): it's safe to use [to_result_exn]. At least,
             [cur.Prm.state] was set to [Resolved _] or the state is different
             than [Pending]. [to_result_exn] fails only when we have the
             [Pending] case. *)
          Prm.to_result_exn (Atomic.get cur.Prm.state)
      | _ -> raise Still_has_children
    in
    let exnc = function
      | (Not_a_child | Still_has_children) as exn -> reraise exn
      | exn ->
          (* NOTE(dinosaure): here, we must set the promise to [Failed] and
             expect that the user [cancel]led or [await]ed the task.
             TODO(dinosaure): we probably should use [compare_and_set] instead
             of [set] to be sure to not overlap a [Failed]/[Resolved] state. *)
          Atomic.set cur.Prm.state (Failed exn);
          Error exn
    in
    let effc :
        type c a. c Effect.t -> ((c, a) Effect.Deep.continuation -> a) option =
     fun eff ->
      (* NOTE(dinosaure): we must be preemptive on the [Failed] case. This case
         mainly appear when the parent was cancelled. [cancel] set our state to
         [Failed Cancelled] and we must [discontinue] the current process. *)
      match (Atomic.get cur.state, eff) with
      | Prm.Consumed (Error exn), _ | Prm.Failed exn, _ ->
          let continuation k =
            (* NOTE(dinosaure): children's children should be cancelled too. *)
            assert (collect_pending_children cur = []);
            Effect.Deep.discontinue k exn
          in
          Some continuation
      | _, Uid.Uid -> Some (get_uid dom0)
      | _, Prm.Spawn (prm, fn) -> Some (spawn ~parent:cur dom0 { go } prm fn)
      | _, Sysc.Syscall prm -> Some (syscall ~parent:cur dom0 { go } prm)
      | _, Prm.Yield ->
          let continuation k =
            runner dom0 { go } cur;
            Effect.Deep.continue k ()
          in
          Some continuation
      | _, Prm.Await prm -> Some (await ~parent:cur dom0 { go } prm)
      | _, Prm.Await_first prms ->
          Some (await_first ~parent:cur dom0 { go } prms)
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
    ; state= Atomic.make (Prm.Pending : _ Prm.private_state)
    ; kind= Prm.Domain
    ; children= Tq.make ()
    ; resources= Tq.make ()
    }
  in
  let dom0 =
    {
      g
    ; todo= Rlist.make g
    ; domain= uid
    ; events
    ; wait= (Mutex.create (), Condition.create ())
    }
  in
  go dom0 prm fn

let run ?g ?events fn =
  match run ?events ?g fn with Ok v -> v | Error exn -> raise exn

let syscall sys fn = Syscall (sys, fn)
