exception Invalid_syscall
exception Not_owner

module Queue = Queue

module Domain_id = struct
  type t = int

  let null = 0
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int

  type _ Effect.t += Domain_id : t Effect.t

  let self () = Effect.perform Domain_id

  let gen, reset =
    let value = Atomic.make (null + 1) in
    let gen () = Atomic.fetch_and_add value 1 in
    let reset () = Atomic.set value 0 in
    (gen, reset)
end

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

let or_raise = function Ok value -> value | Error exn -> raise exn

module Ty = struct
  type 'a t = Task | Block of (unit -> 'a)
end

module Ownership = struct
  type t =
    | Resource : {
          uid: int
        ; value: 'a
        ; finally: 'a -> unit
        ; active: bool Atomic.t
      }
        -> t

  let gen =
    let value = Atomic.make 0 in
    fun () -> Atomic.fetch_and_add value 1

  let make ~finally value =
    let uid = gen () in
    Resource { uid; value; finally; active= Atomic.make true }

  let finalise (Resource { value; finally; active; _ }) =
    if Atomic.get active then
      try finally value with exn -> raise (Fun.Finally_raised exn)

  type _ Effect.t += Ownership : t -> t Effect.t
  type _ Effect.t += Disown : t -> unit Effect.t
  type _ Effect.t += Transfer : t -> unit Effect.t
  type _ Effect.t += Check : t -> unit Effect.t

  let own ~finally v =
    let resource = make ~finally v in
    Effect.perform (Ownership resource)

  let disown resource = Effect.perform (Disown resource)
  let transfer resource = Effect.perform (Transfer resource)
  let check resource = Effect.perform (Check resource)

  let copy (Resource { uid; value; finally; _ }) =
    Resource { uid; value; finally; active= Atomic.make true }
end

module Promise = struct
  exception Cancelled
  exception Unresolvable_promise

  type 'a pstate =
    | Pending
    | Resolved of 'a
    | Failed of exn
    | Consumed of ('a, exn) result

  type runner = On_hold | Chosen of Domain_id.t

  type 'a t = {
      uid: Id.t
    ; runner: runner Atomic.t
    ; ty: 'a Ty.t
    ; state: 'a pstate Atomic.t
    ; children: w Queue.t
    ; resources: Ownership.t Queue.t
    ; mutable parent: w option
  }
  (* NOTE(dinosaure): we can do a transition state on [state] only into the
     domain which is running the associated task. This means that another domain
     that owns the promise but is not executing the task **cannot** change the
     internal [state] of the promise. However, there is a special case when the
     task associated with the promise has still not been assigned to a domain.
     In this case, the domain can modify the state but **must use the mutex**
     that protects the pool.

     The [domain] field can only be introspected with a mutex (to be truly
     synchronised) and can only be modified through this same mutex.

     This raises the question of the use of [Atomic], which is probably becoming
     obsolete in view of all these synchronisation mechanisms. However, to
     ensure that there are no conflicts between domains, we prefer to use
     [Atomic]. *)

  and w = Promise : 'a t -> w
  and 'a syscall = 'a t
  (* NOTE(dinosaure): it's required to provide a new type [syscall] which limits
     the user to use only [make], [suspend] and [uid] on it. For instance, it's
     impossible to use [await_{first,one,all}] on such value because we can only
     suspend it. *)

  and 'a orphans = 'a t Rcdll.t

  let pp ppf { runner; uid; state; _ } =
    let pp_runner ppf = function
      | On_hold -> Format.pp_print_string ppf "?"
      | Chosen did -> Format.fprintf ppf "%d" did
    in
    let pp_state ppf = function
      | Pending -> Format.pp_print_string ppf "pending"
      | Resolved _ | Consumed (Ok _) -> Format.pp_print_string ppf "resolved"
      | Failed _ | Consumed (Error _) -> Format.pp_print_string ppf "failed"
    in
    Format.fprintf ppf "[%a:%a](%a)" pp_runner (Atomic.get runner) Id.pp uid
      pp_state (Atomic.get state)

  type _ Effect.t +=
    | Local_spawn :
        'a orphans option * Ownership.t list * (unit -> 'a)
        -> 'a t Effect.t

  type _ Effect.t +=
    | Parallel_spawn :
        'a orphans option * Ownership.t list * (unit -> 'a)
        -> 'a t Effect.t

  type _ Effect.t +=
    | Syscall_spawn : Ownership.t list * (unit -> 'a) -> 'a syscall Effect.t

  type _ Effect.t += Await : 'a t -> ('a, exn) result Effect.t
  type _ Effect.t += Cancel : 'a t -> unit Effect.t
  type _ Effect.t += Await_one : bool * 'a t list -> ('a, exn) result Effect.t
  type _ Effect.t += Await_all : 'a t list -> ('a, exn) result list Effect.t
  type _ Effect.t += Suspend : 'a syscall -> ('a, exn) result Effect.t
  type _ Effect.t += Multiple_spawn : (unit -> 'a) list -> 'a t list Effect.t

  let call_cc ?orphans ?(give = []) fn =
    Effect.perform (Local_spawn (orphans, give, fn))

  let call ?orphans ?(give = []) fn =
    Effect.perform (Parallel_spawn (orphans, give, fn))

  let parallel lst = Effect.perform (Multiple_spawn lst)
  let await prm = Effect.perform (Await prm)
  let await_exn prm = or_raise (await prm)
  let make ?(give = []) return = Effect.perform (Syscall_spawn (give, return))
  let suspend prm = Effect.perform (Suspend prm)
  let cancel prm = Effect.perform (Cancel prm)

  let await_first = function
    | [] -> invalid_arg "Promise.await_first"
    | prms -> Effect.perform (Await_one (true, prms))

  let await_one = function
    | [] -> invalid_arg "Promise.await_one"
    | prms -> Effect.perform (Await_one (false, prms))

  let await_all = function
    | [] -> invalid_arg "Promise.await_all"
    | prms -> Effect.perform (Await_all prms)

  (* XXX(dinosaure): transition state operations. *)

  let rec to_consumed_with prm res =
    if not (Atomic.compare_and_set prm.state Pending (Consumed res)) then
      match Atomic.get prm.state with
      | (Resolved _ | Failed _) as seen ->
          if not (Atomic.compare_and_set prm.state seen (Consumed res)) then
            to_consumed_with prm res
      | _ -> ()

  let rec to_consumed prm =
    match Atomic.get prm.state with
    | Resolved v as seen ->
        if not (Atomic.compare_and_set prm.state seen (Consumed (Ok v))) then
          to_consumed prm
        else Ok v
    | Failed exn as seen ->
        if not (Atomic.compare_and_set prm.state seen (Consumed (Error exn)))
        then to_consumed prm
        else Error exn
    | Consumed res -> res
    | Pending -> raise Unresolvable_promise

  let to_resolved prm value =
    let set = Atomic.compare_and_set prm.state Pending (Resolved value) in
    assert set

  let to_failed prm exn =
    let set = Atomic.compare_and_set prm.state Pending (Failed exn) in
    assert set

  (* XXX(dinosaure): state introspections. *)

  let to_result_exn prm =
    match Atomic.get prm.state with
    | Resolved v -> Ok v
    | Failed exn -> Error exn
    | Consumed r -> r
    | Pending -> invalid_arg "Promise.to_result_exn"

  let rec is_consumed : type a. a t -> bool =
   fun prm ->
    match Atomic.get prm.state with
    | Consumed _ ->
        let res = ref true in
        let f (Promise prm) = res := is_consumed prm && !res in
        Queue.iter ~f prm.children; !res
    | _ -> false

  let is_pending prm = Atomic.get prm.state = Pending
  let uid { uid; _ } = uid
  let orphans () = Rcdll.make (Random.State.make_self_init ())

  let care orphans =
    let res = ref None in
    let f node =
      let prm = Rcdll.data node in
      match (is_pending prm, !res) with
      | false, None ->
          res := Some prm;
          Rcdll.remove node
      | _ -> ()
    in
    Rcdll.iter_on ~f orphans; !res

  type 'a state = Resolved of 'a | Failed of exn | Pending

  let state prm =
    match Atomic.get prm.state with
    | Consumed (Ok v) | Resolved v -> Resolved v
    | Consumed (Error exn) | Failed exn -> Failed exn
    | Pending -> Pending
end

type 'a continuation = (('a, exn) result, unit) Effect.Deep.continuation

module Local = struct
  (* XXX(dinosaure): a local {task,unblock,cancel} is a task which was taken
     by a specific domain. An invariant exists: [prm.Promise.domain = Chose _]. *)

  type 'a unblock = {
      prm: 'a Promise.t
    ; fn: unit -> unit
    ; return: unit -> 'a
    ; k: 'a continuation
  }

  type t =
    | Task : 'a Promise.t * (unit -> 'a) -> t
    | Unblock : 'a unblock -> t
    | Cancel : 'a Promise.t -> t

  let prm = function
    | Task (prm, _) -> Promise.Promise prm
    | Unblock { prm; _ } -> Promise.Promise prm
    | Cancel prm -> Promise.Promise prm

  type block = Block : 'a Promise.t * (unit -> 'a) * 'a continuation -> block
end

module Shared = struct
  (* XXX(dinosaure): a shared {task,cancel} is a task which exists into our pool
     and was not taken yet by a domain. *)

  type t =
    | Task : Domain_id.t * 'a Promise.t * (unit -> 'a) -> t
    | Dedicated_task : Domain_id.t * 'a Promise.t * (unit -> 'a) -> t

  type cancel = Cancel : 'a Promise.t -> cancel
end

type task = Task : 'a Promise.t * (unit -> unit) -> task
type events = { interrupt: unit -> unit; select: unit -> task list }

type domain = {
    g: Random.State.t
  ; llist: Local.t Rcdll.t
  ; blist: Local.block Rcdll.t
  ; uid: Domain_id.t
  ; events: events
  ; suspended_once: bool Atomic.t
}

type pool = {
    glist: Shared.t Rcdll.t
  ; clist: Shared.cancel Rcdll.t
  ; work: Mutex.t * Condition.t
  ; working: Condition.t
  ; working_counter: int Atomic.t
  ; domains_counter: int Atomic.t
  ; step: Mutex.t * Condition.t
  ; idle: bool array
  ; stop: bool Atomic.t
  ; domains: domain array
}

type go = { go: 'a. pool -> domain -> 'a Promise.t -> (unit -> 'a) -> unit }
[@@unboxed]

let really_ignore fn = try ignore (fn ()) with _ -> ()

let interrupt_domain ~uid pool =
  Array.iter
    (fun domain -> if domain.uid = uid then domain.events.interrupt ())
    pool.domains

let await_any_domains domain pool =
  Mutex.lock (fst pool.step);
  let uid = ref 0 in
  let is_sleeping value =
    incr uid;
    if !uid = domain.uid then true else value
  in
  if not (Array.for_all is_sleeping pool.idle) then
    Condition.wait (snd pool.step) (fst pool.step);
  Mutex.unlock (fst pool.step)
(* XXX(dinosaure): if all domains sleep, we don't wait something from them.
   Otherwise, we wait a signal from one of them. We must be sure that we don't
   wait a signal from ouselves so we consider our current domain as a sleeping
   domain (even if it's false because we run this function into the current
   domain). *)

let remove_prm_from_pool pool prm =
  let f node =
    let (Promise.Promise prm') =
      match Rcdll.data node with
      | Shared.Task (_, prm', _) -> Promise.Promise prm'
      | Shared.Dedicated_task (_, prm', _) -> Promise.Promise prm'
    in
    if prm'.Promise.uid = prm.Promise.uid then Rcdll.remove node
  in
  Rcdll.iter_on ~f pool.glist;
  (* NOTE(dinosaure): it's safe to do a transition state here because
     the task associated to the promise is not executed by any one domain.
     Also, due to the mutex protection, no domain can take the ownership
     on this specific cancellation job. *)
  Promise.to_consumed_with prm (Error Promise.Cancelled)
(* XXX(dinosaure): [remove_prm_from_pool] must be used with a mutex. *)

let rec until_children_are_cancelled pool domain = function
  | [] -> ()
  | unresolved ->
      let unresolved, _ =
        List.partition
          (fun (Promise.Promise prm) -> Promise.is_pending prm)
          unresolved
      in
      if unresolved <> [] then await_any_domains domain pool;
      until_children_are_cancelled pool domain unresolved
(* NOTE(dinosaure): we did not fully described [until_children_are_cancelled]
   and the usage of this function is probably really specific to how we cancel
   a promise and its children. Currently, this function is called after all the
   children in the same domain have been cleaned - so we don't need to call
   [Run.run_local] or discriminate promises into the same domain and promises
   into another domain. Only promises that run in another domain should
   therefore remain, hence the [await_any_domains]. *)

module Run = struct
  (* NOTE(dinosaure): even if we [really_ignore] the result of a task (even its
     exception), [go] cares about such result - we don't want that our domain
     fails randomly when it tries to execute a task. So we can [really_ignore]
     because [go] cares about the result. *)

  let rec step pool domain go = function
    | Local.Task (prm, fn) ->
        if Atomic.get prm.Promise.state = Pending then (
          ignore
            (Atomic.compare_and_set prm.Promise.runner Promise.On_hold
               (Promise.Chosen domain.uid));
          really_ignore (fun () -> go.go pool domain prm fn))
    | Local.Unblock { prm; fn; return; k } ->
        really_ignore (fun () ->
            go.go pool domain prm (fun () -> fn (); return ()));
        (* NOTE(dinosaure): it's safe to consider that the user [await]/[suspend]
           the promise. The apparition of [Unblock] is only due to an explicit
           call to [suspend] - the user is actually waiting for a result, and
           we resume the continuation with the result. *)
        let res = Promise.to_consumed prm in
        Effect.Deep.continue k res
    | Local.Cancel prm ->
        let f0 node =
          let (Promise.Promise prm') = Local.prm (Rcdll.data node) in
          if prm'.Promise.uid = prm.Promise.uid then Rcdll.remove node
        in
        let f1 node =
          let (Local.Block (prm', _, _)) = Rcdll.data node in
          if prm'.Promise.uid = prm.Promise.uid then Rcdll.remove node
        in
        Rcdll.iter_on ~f:f0 domain.llist;
        Rcdll.iter_on ~f:f1 domain.blist;
        (* XXX(dinosaure): we must recurse on [prm]'s children here because this
           this is the only point where [prm.Promise.children] is up to date -
           because we are currently into the same domain where we (will?)
           tr{y,ied} to run the task associated to [prm]. *)
        Queue.iter
          ~f:(fun (Promise.Promise prm) ->
            match
              (Atomic.get prm.Promise.state, Atomic.get prm.Promise.runner)
            with
            | Promise.Pending, Promise.Chosen uid when uid = domain.uid ->
                step pool domain go (Local.Cancel prm)
            | Promise.Pending, Promise.Chosen uid ->
                Mutex.lock (fst pool.work);
                Rcdll.push (Shared.Cancel prm) pool.clist;
                interrupt_domain ~uid pool;
                Condition.broadcast (snd pool.work);
                Mutex.unlock (fst pool.work)
            | Promise.Pending, Promise.On_hold ->
                Mutex.lock (fst pool.work);
                remove_prm_from_pool pool prm;
                Mutex.unlock (fst pool.work)
            | _ -> ())
          prm.Promise.children;
        until_children_are_cancelled pool domain
          (Queue.to_list prm.Promise.children);
        Queue.iter
          ~f:(fun resource -> Ownership.finalise resource)
          prm.Promise.resources;
        Promise.to_consumed_with prm (Error Promise.Cancelled)

  let transfer domain processes =
    let go (Task (prm, fn)) =
      let res = ref None in
      let f node =
        let (Local.Block (prm', _, _)) = Rcdll.data node in
        if Id.equal prm.Promise.uid prm'.Promise.uid then res := Some node
      in
      Rcdll.iter_on ~f domain.blist;
      match !res with
      | Some node ->
          let (Local.Block (prm, return, k)) = Rcdll.data node in
          let unblock = { Local.prm; fn; return; k } in
          Rcdll.remove node;
          Rcdll.push (Local.Unblock unblock) domain.llist
      | None -> raise Invalid_syscall
    in
    List.iter go processes

  let rec run_local pool domain go =
    match Rcdll.take domain.llist with
    | process -> step pool domain go process
    | exception Rcdll.Empty -> (
        match domain.events.select () with
        | [] -> Domain.cpu_relax ()
        | processes -> transfer domain processes; run_local pool domain go)

  let with_lock m fn = Mutex.lock m; fn (); Mutex.unlock m

  let run pool domain go process =
    with_lock (fst pool.step) (fun () -> pool.idle.(pred domain.uid) <- false);
    Option.iter (step pool domain go) process;
    run_local pool domain go;
    with_lock (fst pool.step) @@ fun () ->
    pool.idle.(pred domain.uid) <- true;
    Condition.broadcast (snd pool.step)
end

exception Still_has_children
exception Not_a_child
exception No_domain_available
exception Resource_leak

module Pool = struct
  let assign_domain domain prm =
    Atomic.compare_and_set prm.Promise.runner Promise.On_hold
      (Promise.Chosen domain.uid)
    |> fun set -> assert set

  let nothing_to_do pool domain =
    Rcdll.is_empty pool.glist
    && Rcdll.is_empty pool.clist
    && Rcdll.is_empty domain.blist
  (* XXX(dinosaure): [nothing_to_do] must be used with a mutex. *)

  let locate pool domain =
    let to_cancel = ref None in
    let f node =
      let (Shared.Cancel prm) = Rcdll.data node in
      match (Atomic.get prm.Promise.runner, !to_cancel) with
      | Promise.On_hold, _ -> remove_prm_from_pool pool prm
      | Promise.Chosen uid, None when uid = domain.uid ->
          Rcdll.remove node;
          to_cancel := Some (Promise.Promise prm)
      | _ -> ()
    in
    Rcdll.iter_on ~f pool.clist;
    match !to_cancel with
    | Some (Promise.Promise prm) -> Some (Local.Cancel prm)
    | None when Rcdll.is_empty domain.blist -> (
        (* XXX(dinosaure): if the domain has some blockers, we don't locate
           a pending global task and let it to handle its own blockers task. *)
        match Rcdll.take pool.glist with
        | Shared.Task (launcher, prm, fn) when launcher <> domain.uid ->
            assign_domain domain prm;
            Some (Local.Task (prm, fn))
        | Shared.Dedicated_task (runner, prm, fn) as task ->
            if runner = domain.uid then Some (Local.Task (prm, fn))
            else (Rcdll.push task pool.glist; None)
        | task -> Rcdll.push task pool.glist; None
        | exception Rcdll.Empty -> None)
    | None -> None
  (* XXX(dinosaure): [locate] must be used with a mutex. *)

  let worker pool domain go =
    let exception Exit in
    try
      while true do
        Mutex.lock (fst pool.work);
        while nothing_to_do pool domain && not (Atomic.get pool.stop) do
          Condition.wait (snd pool.work) (fst pool.work)
        done;
        if Atomic.get pool.stop then raise Exit;
        let local_process = locate pool domain in
        ignore (Atomic.fetch_and_add pool.working_counter 1);
        Mutex.unlock (fst pool.work);
        Run.run pool domain go local_process;
        Mutex.lock (fst pool.work);
        ignore (Atomic.fetch_and_add pool.working_counter (-1));
        if
          (not (Atomic.get pool.stop))
          && Atomic.get pool.working_counter = 0
          && nothing_to_do pool domain
        then Condition.signal pool.working;
        Mutex.unlock (fst pool.work)
      done
    with Exit ->
      ignore (Atomic.fetch_and_add pool.domains_counter (-1));
      Condition.signal pool.working;
      Mutex.unlock (fst pool.work)

  let push pool ~launcher ~fn ?domain prm =
    Mutex.lock (fst pool.work);
    (match domain with
    | None -> Rcdll.push (Shared.Task (launcher.uid, prm, fn)) pool.glist
    | Some uid -> Rcdll.push (Shared.Dedicated_task (uid, prm, fn)) pool.glist);
    Condition.broadcast (snd pool.work);
    Mutex.unlock (fst pool.work)

  let wait pool =
    let exception Exit in
    Mutex.lock (fst pool.work);
    try
      while true do
        if
          ((not (Atomic.get pool.stop)) && Atomic.get pool.working_counter <> 0)
          || (Atomic.get pool.stop && Atomic.get pool.domains_counter <> 0)
        then Condition.wait pool.working (fst pool.work)
        else raise Exit
      done
    with Exit -> Mutex.unlock (fst pool.work)

  let kill pool =
    Mutex.lock (fst pool.work);
    Rcdll.drop pool.glist;
    (* XXX(dinosaure): [glist] should be empty. *)
    Atomic.set pool.stop true;
    Condition.broadcast (snd pool.work);
    Mutex.unlock (fst pool.work);
    wait pool

  let domain ~events ?(g = Random.State.make_self_init ()) idx =
    {
      g
    ; llist= Rcdll.make g
    ; blist= Rcdll.make g
    ; uid= Domain_id.gen ()
    ; suspended_once= Atomic.make false
    ; events= events (succ idx)
    }

  let make ?(g = Random.State.make_self_init ())
      ?(domains = max 0 (Domain.recommended_domain_count () - 1)) ~events go =
    let domains = List.init domains (domain ~events ~g:(Random.State.copy g)) in
    let pool =
      {
        glist= Rcdll.make g
      ; clist= Rcdll.make g
      ; work= (Mutex.create (), Condition.create ())
      ; working= Condition.create ()
      ; working_counter= Atomic.make 0
      ; domains_counter= Atomic.make (List.length domains)
      ; step= (Mutex.create (), Condition.create ())
      ; idle= Array.make (List.length domains) true
      ; stop= Atomic.make false
      ; domains= Array.of_list domains
      }
    in
    let spawn domain = Domain.spawn (fun () -> worker pool domain go) in
    (pool, List.map spawn domains)
end

type _ Effect.t += Yield : unit Effect.t

let yield () = Effect.perform Yield

let do_local_spawn domain ress ?orphans ~parent fn k =
  let resources = Queue.make () in
  let f (Ownership.Resource { active; _ } as resource) =
    Queue.enqueue resources (Ownership.copy resource);
    Atomic.set active false
  in
  List.iter f ress;
  let prm =
    {
      Promise.uid= Id.gen ()
    ; runner= Atomic.make (Promise.Chosen domain.uid)
    ; state= Atomic.make (Promise.Pending : _ Promise.pstate)
    ; ty= Ty.Task
    ; children= Queue.make ()
    ; resources
    ; parent= Some (Promise.Promise parent)
    }
  in
  Option.iter (Rcdll.push prm) orphans;
  Queue.enqueue parent.Promise.children (Promise.Promise prm);
  Rcdll.push (Local.Task (prm, fn)) domain.llist;
  Effect.Deep.continue k prm

let do_parallel_spawn pool domain ress ?orphans ~parent fn k =
  if Array.length pool.domains <= 0 then
    Effect.Deep.discontinue k No_domain_available
  else
    let resources = Queue.make () in
    let f (Ownership.Resource { active; _ } as resource) =
      Queue.enqueue resources (Ownership.copy resource);
      Atomic.set active false
    in
    List.iter f ress;
    let prm =
      {
        Promise.uid= Id.gen ()
      ; runner= Atomic.make Promise.On_hold
      ; state= Atomic.make (Promise.Pending : _ Promise.pstate)
      ; ty= Ty.Task
      ; children= Queue.make ()
      ; resources
      ; parent= Some (Promise.Promise parent)
      }
    in
    Option.iter (Rcdll.push prm) orphans;
    Queue.enqueue parent.Promise.children (Promise.Promise prm);
    Pool.push pool ~launcher:domain ~fn prm;
    Effect.Deep.continue k prm

let do_multiple_spawns pool domain ~parent lst k =
  if List.length lst > Array.length pool.domains then
    Effect.Deep.discontinue k (Invalid_argument "Miou.parallel");
  let uids =
    List.filter_map
      (fun domain' ->
        if domain.uid = domain'.uid then None else Some domain'.uid)
      (Array.to_list pool.domains)
  in
  let _, prms =
    List.fold_left
      (fun (uids, prms) fn ->
        let[@warning "-8"] (uid :: uids) = uids in
        let prm =
          {
            Promise.uid= Id.gen ()
          ; runner= Atomic.make Promise.On_hold
          ; state= Atomic.make (Promise.Pending : _ Promise.pstate)
          ; ty= Ty.Task
          ; children= Queue.make ()
          ; resources= Queue.make ()
          ; parent= Some (Promise.Promise parent)
          }
        in
        Queue.enqueue parent.Promise.children (Promise.Promise prm);
        Pool.push pool ~launcher:domain ~domain:uid ~fn prm;
        (uids, prm :: prms))
      (uids, []) lst
  in
  Effect.Deep.continue k prms

let is_a_child_of ~parent prm =
  let res = ref false in
  let f (Promise.Promise child) = res := !res || child.uid = prm.Promise.uid in
  Queue.iter ~f parent.Promise.children;
  !res

let rec until_is_resolved pool domain go ~parent prm k =
  match (prm.Promise.ty, Atomic.get prm.Promise.state) with
  | ( _
    , Promise.Consumed
        (Error ((Still_has_children | Not_a_child | Resource_leak) as exn)) ) ->
      Effect.Deep.discontinue k exn
  | ( _
    , Promise.Failed ((Still_has_children | Not_a_child | Resource_leak) as exn)
    ) ->
      Effect.Deep.discontinue k exn
  | _, Promise.Consumed res -> Effect.Deep.continue k res
  | _, Promise.(Resolved _ | Failed _) ->
      Effect.Deep.continue k (Promise.to_consumed prm)
  | Ty.Block return, Promise.Pending ->
      (* XXX(dinosaure): actually [do_suspend] do this. *)
      Rcdll.push (Local.Block (prm, return, k)) domain.blist
  | Ty.Task, Promise.Pending -> (
      match Atomic.get prm.Promise.runner with
      | Promise.On_hold -> until_is_resolved pool domain go ~parent prm k
      | Promise.Chosen uid when domain.uid = uid ->
          Run.run_local pool domain go;
          until_is_resolved pool domain go ~parent prm k
      | Promise.Chosen _ ->
          await_any_domains domain pool;
          until_is_resolved pool domain go ~parent prm k)

let do_await pool domain go ~parent prm k =
  if not (is_a_child_of ~parent prm) then Effect.Deep.discontinue k Not_a_child;
  until_is_resolved pool domain go ~parent prm k

let do_suspend domain ~parent prm k =
  if not (is_a_child_of ~parent prm) then Effect.Deep.discontinue k Not_a_child;
  if Promise.is_pending prm then begin
    (* XXX(dinosaure): [do_suspend] should be run by a [Suspend] which only
       accepts promise with [Ty.Block]. *)
    let[@warning "-8"] (Ty.Block return) = prm.Promise.ty in
    Atomic.set domain.suspended_once true;
    Rcdll.push (Local.Block (prm, return, k)) domain.blist
  end

let do_yield pool domain go k =
  Run.run_local pool domain go;
  Array.iter (fun domain -> interrupt_domain ~uid:domain.uid pool) pool.domains;
  Effect.Deep.continue k ()

let rec until_is_cancelled pool domain go prm =
  match (prm.Promise.ty, Atomic.get prm.Promise.state) with
  | _, Promise.Consumed _ -> ()
  | _, Promise.(Resolved _ | Failed _) -> until_is_cancelled pool domain go prm
  | _, Promise.Pending -> (
      match Atomic.get prm.Promise.runner with
      | Promise.On_hold -> until_is_cancelled pool domain go prm
      | Promise.Chosen uid when domain.uid = uid ->
          Run.run_local pool domain go;
          until_is_cancelled pool domain go prm
      | Promise.Chosen _ ->
          await_any_domains domain pool;
          until_is_cancelled pool domain go prm)

let rec cancel' : type a. pool -> domain -> go -> a Promise.t -> unit =
 fun pool domain go prm ->
  (* NOTE(dinosaure): for the task which runs into the same domain than the
     canceller, we do what [Run.step] does but locally:
     - cancel children (and redo the operation)
     - release resources
     - definitely remove the task from lists

     When we have [Promise.On_hold], the task did not run yet (and its children), we
     just need to delete everything from global lists. When we have
     [Promise.Chosen], a part of the task was running and some of its children
     (which appeared into another domain). We must cancel them and wait them. *)
  match (Atomic.get prm.Promise.state, Atomic.get prm.Promise.runner) with
  | Promise.Consumed _, _ -> ()
  | _, Promise.On_hold ->
      Queue.iter
        ~f:(fun (Promise.Promise prm) -> cancel' pool domain go prm)
        prm.Promise.children;
      Queue.iter
        ~f:(fun resource -> Ownership.finalise resource)
        prm.Promise.resources;
      remove_prm_from_pool pool prm
  | _, Promise.Chosen uid when uid = domain.uid ->
      (* XXX(dinosaure): this code is safe because we are surrounded by a
         lock and the task can only be executed in the current domain, which
         is already busy cancelling this task. *)
      let f0 node =
        let (Promise.Promise prm') = Local.prm (Rcdll.data node) in
        if prm'.Promise.uid = prm.Promise.uid then Rcdll.remove node
      in
      let f1 node =
        let (Local.Block (prm', _, _)) = Rcdll.data node in
        if prm'.Promise.uid = prm.Promise.uid then Rcdll.remove node
      in
      Rcdll.iter_on ~f:f0 domain.llist;
      Rcdll.iter_on ~f:f1 domain.blist;
      Queue.iter
        ~f:(fun (Promise.Promise prm) -> cancel' pool domain go prm)
        prm.Promise.children;
      until_children_are_cancelled pool domain
        (Queue.to_list prm.Promise.children);
      Queue.iter
        ~f:(fun resource -> Ownership.finalise resource)
        prm.Promise.resources;
      Promise.to_consumed_with prm (Error Promise.Cancelled)
  | _, Promise.Chosen uid ->
      (* XXX(dinosaure): this code is like [Pool.cancel] but we are already
         surrounded by the lock of [fst pool.work]. *)
      Rcdll.push (Shared.Cancel prm) pool.clist;
      interrupt_domain ~uid pool;
      Condition.broadcast (snd pool.work)

and cancel : type a. pool -> domain -> go -> a Promise.t -> unit =
 fun pool domain go prm ->
  Mutex.lock (fst pool.work);
  cancel' pool domain go prm;
  Mutex.unlock (fst pool.work);
  until_is_cancelled pool domain go prm

let do_cancel pool domain go ~parent prm k =
  if not (is_a_child_of ~parent prm) then Effect.Deep.discontinue k Not_a_child;
  cancel pool domain go prm;
  Effect.Deep.continue k ()

let do_syscall_spawn domain ress ~parent ~return k =
  let resources = Queue.make () in
  let f (Ownership.Resource { active; _ } as resource) =
    Queue.enqueue resources (Ownership.copy resource);
    Atomic.set active false
  in
  List.iter f ress;
  let prm =
    {
      Promise.uid= Id.gen ()
    ; runner= Atomic.make (Promise.Chosen domain.uid)
    ; state= Atomic.make (Promise.Pending : _ Promise.pstate)
    ; ty= Ty.Block return
    ; children= Queue.make ()
    ; resources= Queue.make ()
    ; parent= Some (Promise.Promise parent)
    }
  in
  if Promise.is_pending parent then (
    Queue.enqueue parent.Promise.children (Promise.Promise prm);
    Effect.Deep.continue k prm)
  else Effect.Deep.discontinue k Promise.Cancelled

let discontinue_if_invalid k prm =
  match Atomic.get prm.Promise.state with
  | Consumed (Error ((Still_has_children | Not_a_child) as exn))
  | Failed ((Still_has_children | Not_a_child) as exn) ->
      Effect.Deep.discontinue k exn;
      true
  | _ -> false

let do_await_one ~and_cancel pool domain go ~parent prms k =
  if not (List.for_all (is_a_child_of ~parent) prms) then
    Effect.Deep.discontinue k Not_a_child;
  let rec until () =
    let unresolved, resolved = List.partition Promise.is_pending prms in
    match resolved with
    | [] ->
        Run.run_local pool domain go;
        let run_on_another_domain =
          List.exists
            (fun prm ->
              match Atomic.get prm.Promise.runner with
              | Promise.On_hold -> false
              | Promise.Chosen uid -> uid <> domain.uid)
            unresolved
        in
        if run_on_another_domain then await_any_domains domain pool;
        until ()
    | resolved when List.exists (discontinue_if_invalid k) resolved ->
        if and_cancel then List.iter (cancel pool domain go) unresolved
    | resolved when and_cancel ->
        let resolved = List.map Promise.to_consumed resolved in
        let len = List.length resolved in
        let idx = Random.State.int domain.g len in
        let res = List.nth resolved idx in
        List.iter (fun prm -> cancel pool domain go prm) unresolved;
        Effect.Deep.continue k res
    | resolved ->
        let len = List.length resolved in
        let idx = Random.State.int domain.g len in
        let prm = List.nth resolved idx in
        Effect.Deep.continue k (Promise.to_consumed prm)
  in
  until ()

let do_await_all pool domain go ~parent prms k =
  if not (List.for_all (is_a_child_of ~parent) prms) then
    Effect.Deep.discontinue k Not_a_child;
  let rec until = function
    | [] -> Effect.Deep.continue k (List.map Promise.to_consumed prms)
    | unresolved ->
        Run.run_local pool domain go;
        await_any_domains domain pool;
        let unresolved, resolved =
          List.partition Promise.is_pending unresolved
        in
        if not (List.exists (discontinue_if_invalid k) resolved) then
          until unresolved
  in
  until prms

let do_own prm (Ownership.Resource { uid; _ } as resource) k =
  let exists = ref false in
  let f (Ownership.Resource { uid= uid'; _ }) =
    exists := !exists || uid = uid'
  in
  Queue.iter ~f prm.Promise.resources;
  if not !exists then Queue.enqueue prm.Promise.resources resource;
  Effect.Deep.continue k resource

let do_disown prm (Ownership.Resource { uid; _ }) k =
  let f (Ownership.Resource { uid= uid'; active; _ }) =
    if uid = uid' then Atomic.set active false
  in
  Queue.iter ~f prm.Promise.resources;
  Effect.Deep.continue k ()

let do_transfer prm (Ownership.Resource { uid; _ } as resource) k =
  match prm.Promise.parent with
  | Some (Promise.Promise parent) ->
      let f (Ownership.Resource { uid= uid'; active; _ }) =
        if uid = uid' then Atomic.set active false
      in
      Queue.iter ~f prm.Promise.resources;
      let exists = ref false in
      let f (Ownership.Resource { uid= uid'; _ }) =
        exists := !exists || uid = uid'
      in
      Queue.iter ~f parent.Promise.resources;
      if not !exists then
        Queue.enqueue parent.Promise.resources (Ownership.copy resource);
      Effect.Deep.continue k ()
  | None ->
      Effect.Deep.continue k
        () (* TODO(dinosaure): discontinue with an exception? *)

let do_check prm (Ownership.Resource { uid; _ }) k =
  let exists = ref false in
  let f (Ownership.Resource { uid= uid'; _ }) =
    exists := !exists || uid = uid'
  in
  Queue.iter ~f prm.Promise.resources;
  if not !exists then Effect.Deep.discontinue k Not_owner
  else Effect.Deep.continue k ()

let collect_pending_children prm =
  let res = ref [] in
  let f (Promise.Promise prm) =
    if not (Promise.is_consumed prm) then res := Promise.Promise prm :: !res
  in
  Queue.iter ~f prm.Promise.children;
  !res

let collect_pending_resources prm =
  let res = ref [] in
  let f (Ownership.Resource { active; _ } as resource) =
    if Atomic.get active then res := resource :: !res
  in
  Queue.iter ~f prm.Promise.resources;
  !res

let run ?(g = Random.State.make_self_init ()) ?domains ~events fn =
  let rec go : type a. pool -> domain -> a Promise.t -> (unit -> a) -> unit =
   fun pool domain prm fn ->
    let retc value =
      match (collect_pending_children prm, collect_pending_resources prm) with
      | [], [] -> Promise.to_resolved prm value
      | prms, [] ->
          List.iter
            (fun (Promise.Promise prm) -> cancel pool domain { go } prm)
            prms;
          Promise.to_consumed_with prm (Error Still_has_children)
      | _, resources ->
          List.iter (fun resource -> Ownership.finalise resource) resources;
          Promise.to_consumed_with prm (Error Resource_leak)
    in
    let exnc exn =
      Queue.iter
        ~f:(fun (Promise.Promise prm) -> cancel pool domain { go } prm)
        prm.Promise.children;
      Queue.iter
        ~f:(fun resource -> Ownership.finalise resource)
        prm.Promise.resources;
      Promise.to_failed prm exn;
      match exn with
      | (Still_has_children | Not_a_child | Resource_leak) as exn -> raise exn
      | _ -> ()
    in
    let effc :
        type a.
        a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option =
     fun eff ->
      match (Atomic.get prm.Promise.state, eff) with
      | Promise.(Failed exn | Consumed (Error exn)), _ ->
          Some (fun k -> Effect.Deep.discontinue k exn)
      | _, Domain_id.Domain_id ->
          Some (fun k -> Effect.Deep.continue k domain.uid)
      | _, Promise.Local_spawn (orphans, ress, fn) ->
          Some (do_local_spawn domain ress ?orphans ~parent:prm fn)
      | _, Promise.Parallel_spawn (orphans, ress, fn) ->
          Some (do_parallel_spawn pool domain ress ?orphans ~parent:prm fn)
      | _, Promise.Syscall_spawn (ress, return) ->
          Some (do_syscall_spawn domain ress ~parent:prm ~return)
      | _, Promise.Multiple_spawn lst ->
          Some (do_multiple_spawns pool domain ~parent:prm lst)
      | _, Promise.Await prm' ->
          Some (do_await pool domain { go } ~parent:prm prm')
      | _, Promise.Suspend prm' -> Some (do_suspend domain ~parent:prm prm')
      | _, Promise.Await_one (and_cancel, prms) ->
          Some (do_await_one ~and_cancel pool domain { go } ~parent:prm prms)
      | _, Promise.Await_all prms ->
          Some (do_await_all pool domain { go } ~parent:prm prms)
      | _, Yield -> Some (do_yield pool domain { go })
      | _, Promise.Cancel prm' ->
          Some (do_cancel pool domain { go } ~parent:prm prm')
      | _, Ownership.Ownership resource -> Some (do_own prm resource)
      | _, Ownership.Disown resource -> Some (do_disown prm resource)
      | _, Ownership.Transfer resource -> Some (do_transfer prm resource)
      | _, Ownership.Check resource -> Some (do_check prm resource)
      | _ -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  let pool, domains =
    Pool.make ~g:(Random.State.copy g) ?domains ~events { go }
  in
  Domain_id.reset ();
  let dom0 =
    {
      g= Random.State.copy g
    ; llist= Rcdll.make g
    ; blist= Rcdll.make g
    ; uid= Domain_id.gen ()
    ; suspended_once= Atomic.make false
    ; events= events 0
    }
  in
  let prm0 =
    {
      Promise.uid= Id.gen ()
    ; runner= Atomic.make (Promise.Chosen dom0.uid)
    ; state= Atomic.make (Promise.Pending : _ Promise.pstate)
    ; ty= Task
    ; children= Queue.make ()
    ; resources= Queue.make ()
    ; parent= None
    }
  in
  Rcdll.push (Local.Task (prm0, fn)) dom0.llist;
  while
    (not (Rcdll.is_empty dom0.llist))
    || (Atomic.get dom0.suspended_once && not (Rcdll.is_empty dom0.blist))
  do
    (* NOTE(dinosaure): if [dom0] was suspended, we must wait something from
       [select ()] to resolve [prm0]. *)
    Run.run_local pool dom0 { go }
  done;
  Pool.kill pool;
  List.iter Domain.join domains;
  Promise.to_result_exn prm0

let always_no_events _ = { interrupt= Fun.const (); select= Fun.const [] }
let task prm fn = Task (prm, fn)

include Promise

let run ?g ?domains ?(events = always_no_events) fn =
  or_raise (run ?g ?domains ~events fn)
