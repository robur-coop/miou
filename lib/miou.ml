[@@@warning "-37"] (* Dedicated_task *)

module Tq = Tq

module Did = struct
  type t = int

  let null = 0
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int

  type _ Effect.t += Did : t Effect.t

  let self () = Effect.perform Did

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

module Own = struct
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

  type _ Effect.t += Own : t -> t Effect.t
  type _ Effect.t += Disown : t -> unit Effect.t

  let own ~finally v =
    let resource = make ~finally v in
    Effect.perform (Own resource)

  let disown resource = Effect.perform (Disown resource)

  let copy (Resource { uid; value; finally; _ }) =
    Resource { uid; value; finally; active= Atomic.make true }
end

module Prm = struct
  exception Cancelled
  exception Unresolvable_promise

  type 'a pstate =
    | Pending
    | Resolved of 'a
    | Failed of exn
    | Consumed of ('a, exn) result

  type domain = On_hold | Chosen of Did.t

  type 'a t = {
      uid: Id.t
    ; domain: domain Atomic.t
    ; ty: 'a Ty.t
    ; state: 'a pstate Atomic.t
    ; children: w Tq.t
    ; resources: Own.t Tq.t
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

  and w = Prm : 'a t -> w
  and 'a syscall = 'a t
  (* NOTE(dinosaure): it's required to provide a new type [syscall] which limits
     the user to use only [make], [suspend] and [uid] on it. For instance, it's
     impossible to use [await_{first,one,all}] on such value because we can only
     suspend it. *)

  and 'a orphans = 'a t L.t

  let pp ppf { domain; uid; state; _ } =
    let pp_domain ppf = function
      | On_hold -> Format.pp_print_string ppf "?"
      | Chosen did -> Format.fprintf ppf "%d" did
    in
    let pp_state ppf = function
      | Pending -> Format.pp_print_string ppf "pending"
      | Resolved _ | Consumed (Ok _) -> Format.pp_print_string ppf "resolved"
      | Failed _ | Consumed (Error _) -> Format.pp_print_string ppf "failed"
    in
    Format.fprintf ppf "[%a:%a](%a)" pp_domain (Atomic.get domain) Id.pp uid
      pp_state (Atomic.get state)

  type _ Effect.t +=
    | Local_spawn :
        'a orphans option * Own.t list * (unit -> 'a)
        -> 'a t Effect.t

  type _ Effect.t +=
    | Parallel_spawn :
        'a orphans option * Own.t list * (unit -> 'a)
        -> 'a t Effect.t

  type _ Effect.t +=
    | Syscall_spawn : Own.t list * (unit -> 'a) -> 'a syscall Effect.t

  type _ Effect.t += Await : 'a t -> ('a, exn) result Effect.t
  type _ Effect.t += Cancel : 'a t -> unit Effect.t
  type _ Effect.t += Await_one : bool * 'a t list -> ('a, exn) result Effect.t
  type _ Effect.t += Await_all : 'a t list -> ('a, exn) result list Effect.t
  type _ Effect.t += Suspend : 'a syscall -> ('a, exn) result Effect.t

  let call_cc ?orphans ?(give = []) fn =
    Effect.perform (Local_spawn (orphans, give, fn))

  let call ?orphans ?(give = []) fn =
    Effect.perform (Parallel_spawn (orphans, give, fn))

  let await prm = Effect.perform (Await prm)
  let await_exn prm = or_raise (await prm)
  let make ?(give = []) return = Effect.perform (Syscall_spawn (give, return))
  let suspend prm = Effect.perform (Suspend prm)
  let cancel prm = Effect.perform (Cancel prm)

  let await_first = function
    | [] -> invalid_arg "Prm.await_first"
    | prms -> Effect.perform (Await_one (true, prms))

  let await_one = function
    | [] -> invalid_arg "Prm.await_one"
    | prms -> Effect.perform (Await_one (false, prms))

  let await_all = function
    | [] -> invalid_arg "Prm.await_all"
    | prms -> Effect.perform (Await_all prms)

  (* XXX(dinosaure): transition state operations. *)

  let rec to_consumed_with prm res =
    if not (Atomic.compare_and_set prm.state Pending (Consumed res)) then
      match Atomic.get prm.state with
      | Resolved v as seen ->
          if not (Atomic.compare_and_set prm.state seen (Consumed (Ok v))) then
            to_consumed_with prm (Ok v)
      | Failed exn as seen ->
          if not (Atomic.compare_and_set prm.state seen (Consumed (Error exn)))
          then to_consumed_with prm (Error exn)
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
    ignore (Atomic.compare_and_set prm.state Pending (Resolved value))

  let to_failed prm exn =
    ignore (Atomic.compare_and_set prm.state Pending (Failed exn))

  (* XXX(dinosaure): state introspections. *)

  let to_result_exn prm =
    match Atomic.get prm.state with
    | Resolved v -> Ok v
    | Failed exn -> Error exn
    | Consumed r -> r
    | Pending -> invalid_arg "Prm.to_result_exn"

  let rec is_consumed : type a. a t -> bool =
   fun prm ->
    match Atomic.get prm.state with
    | Consumed _ ->
        let res = ref true in
        let f (Prm prm) = res := is_consumed prm && !res in
        Tq.iter ~f prm.children; !res
    | _ -> false

  let is_pending prm = Atomic.get prm.state = Pending
  let uid { uid; _ } = uid
  let orphans () = L.make (Random.State.make_self_init ())

  let care orphans =
    let res = ref None in
    let f node =
      let prm = L.data node in
      match (is_pending prm, !res) with
      | false, None ->
          res := Some prm;
          L.remove node
      | _ -> ()
    in
    L.iter_on ~f orphans; !res

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
     by a specific domain. An invariant exists: [prm.Prm.domain = Chose _]. *)

  type 'a unblock = {
      prm: 'a Prm.t
    ; fn: unit -> unit
    ; return: unit -> 'a
    ; k: 'a continuation
  }

  type t =
    | Task : 'a Prm.t * (unit -> 'a) -> t
    | Unblock : 'a unblock -> t
    | Cancel : 'a Prm.t -> t

  let prm = function
    | Task (prm, _) -> Prm.Prm prm
    | Unblock { prm; _ } -> Prm.Prm prm
    | Cancel prm -> Prm.Prm prm

  type block = Block : 'a Prm.t * (unit -> 'a) * 'a continuation -> block
end

module Shared = struct
  (* XXX(dinosaure): a shared {task,cancel} is a task which exists into our pool
     and was not taken yet by a domain. *)

  type t =
    | Task : 'a Prm.t * (unit -> 'a) -> t
    | Dedicated_task : Did.t * 'a Prm.t * (unit -> 'a) -> t

  type cancel = Cancel : 'a Prm.t -> cancel
end

type task = Task : 'a Prm.t * (unit -> unit) -> task
type events = { interrupt: unit -> unit; select: unit -> task list }

type domain = {
    g: Random.State.t
  ; llist: Local.t L.t
  ; blist: Local.block L.t
  ; uid: Did.t
  ; events: events
}

type pool = {
    glist: Shared.t L.t
  ; clist: Shared.cancel L.t
  ; work: Mutex.t * Condition.t
  ; working: Condition.t
  ; working_counter: int Atomic.t
  ; domains_counter: int Atomic.t
  ; step: Mutex.t * Condition.t
  ; idle: bool array
  ; stop: bool Atomic.t
  ; domains: domain array
}

type go = { go: 'a. pool -> domain -> 'a Prm.t -> (unit -> 'a) -> unit }
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
   domain (even if it's probably false). *)

let rec until_children_are_cancelled pool domain = function
  | [] -> ()
  | unresolved ->
      let unresolved, _ =
        List.partition (fun (Prm.Prm prm) -> Prm.is_pending prm) unresolved
      in
      await_any_domains domain pool;
      until_children_are_cancelled pool domain unresolved

module Run = struct
  (* NOTE(dinosaure): even if we [really_ignore] the result of a task (even its
     exception), [go] cares about such result - we don't want that our domain
     fails randomly when it tries to execute a task. So we can [really_ignore]
     because [go] cares about the result. *)

  let rec step pool domain go = function
    | Local.Task (prm, fn) ->
        if Atomic.get prm.Prm.state = Pending then (
          ignore
            (Atomic.compare_and_set prm.Prm.domain Prm.On_hold
               (Prm.Chosen domain.uid));
          really_ignore (fun () -> go.go pool domain prm fn))
    | Local.Unblock { prm; fn; return; k } ->
        really_ignore (fun () ->
            go.go pool domain prm (fun () -> fn (); return ()));
        (* NOTE(dinosaure): it's safe to consider that the user [await]/[suspend]
           the promise. The apparition of [Unblock] is only due to an explicit
           call to [suspend] - the user is actually waiting for a result, and
           we resume the continuation with the result. *)
        let res = Prm.to_consumed prm in
        Effect.Deep.continue k res
    | Local.Cancel prm ->
        let f0 node =
          let (Prm.Prm prm') = Local.prm (L.data node) in
          if prm'.Prm.uid = prm.Prm.uid then L.remove node
        in
        let f1 node =
          let (Local.Block (prm', _, _)) = L.data node in
          if prm'.Prm.uid = prm.Prm.uid then L.remove node
        in
        L.iter_on ~f:f0 domain.llist;
        L.iter_on ~f:f1 domain.blist;
        (* XXX(dinosaure): even if [cancel] recurses on [prm]'s children, during
           the synchronisation mechanism, there may have been a new child that
           was not taken into account. We are currently in the only [prm]
           synchronisation point, so we can take advantage of this to try to
           [cancel] its children again (knowing that it is impossible for the
           user code to add new ones). *)
        Tq.iter
          ~f:(fun (Prm.Prm prm) ->
            match Atomic.get prm.Prm.domain with
            | Prm.Chosen uid when uid = domain.uid ->
                step pool domain go (Local.Cancel prm)
            | Prm.Chosen uid ->
                Mutex.lock (fst pool.work);
                L.push (Shared.Cancel prm) pool.clist;
                interrupt_domain ~uid pool;
                Condition.broadcast (snd pool.work);
                Mutex.unlock (fst pool.work)
            | Prm.On_hold ->
                Mutex.lock (fst pool.work);
                L.push (Shared.Cancel prm) pool.clist;
                Condition.broadcast (snd pool.work);
                Mutex.unlock (fst pool.work))
          prm.Prm.children;
        until_children_are_cancelled pool domain (Tq.to_list prm.Prm.children);
        Prm.to_consumed_with prm (Error Prm.Cancelled)

  let transfer domain processes =
    let go (Task (prm, fn)) =
      let res = ref None in
      let f node =
        let (Local.Block (prm', _, _)) = L.data node in
        if Id.equal prm.Prm.uid prm'.Prm.uid then res := Some node
      in
      L.iter_on ~f domain.blist;
      match !res with
      | Some node ->
          let (Local.Block (prm, return, k)) = L.data node in
          let unblock = { Local.prm; fn; return; k } in
          L.remove node;
          L.push (Local.Unblock unblock) domain.llist
      | None -> ()
    in
    List.iter go processes

  let rec run_local pool domain go =
    match L.take domain.llist with
    | process ->
        step pool domain go process;
        run_local pool domain go
    | exception L.Empty -> (
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
    Atomic.compare_and_set prm.Prm.domain Prm.On_hold (Prm.Chosen domain.uid)
    |> fun set -> assert set

  let remove_prm_from_pool pool prm =
    let f node =
      let (Prm.Prm prm') =
        match L.data node with
        | Shared.Task (prm', _) -> Prm.Prm prm'
        | Shared.Dedicated_task (_, prm', _) -> Prm.Prm prm'
      in
      if prm'.Prm.uid = prm.Prm.uid then L.remove node
    in
    L.iter_on ~f pool.glist;
    Prm.to_consumed_with prm (Error Prm.Cancelled)
  (* XXX(dinosaure): [remove_prm_from_pool] must be used with a mutex. *)

  let nothing_to_do pool = L.is_empty pool.glist && L.is_empty pool.clist
  (* XXX(dinosaure): [nothing_to_do] must be used with a mutex. *)

  let locate pool domain =
    let to_cancel = ref None in
    let f node =
      let (Shared.Cancel prm) = L.data node in
      match (Atomic.get prm.Prm.domain, !to_cancel) with
      | Prm.On_hold, _ -> remove_prm_from_pool pool prm
      | Prm.Chosen uid, None when uid = domain.uid ->
          L.remove node;
          to_cancel := Some (Prm.Prm prm)
      | _ -> ()
    in
    L.iter_on ~f pool.clist;
    match !to_cancel with
    | Some (Prm.Prm prm) -> Some (Local.Cancel prm)
    | None -> (
        match L.take pool.glist with
        | Shared.Task (prm, fn) ->
            assign_domain domain prm;
            Some (Local.Task (prm, fn))
        | Shared.Dedicated_task (did, prm, fn) as task ->
            if did = domain.uid then Some (Local.Task (prm, fn))
            else (L.push task pool.glist; None)
        | exception L.Empty -> None)
  (* XXX(dinosaure): [locate] must be used with a mutex. *)

  let worker pool domain go =
    let exception Exit in
    try
      while true do
        Mutex.lock (fst pool.work);
        while nothing_to_do pool && not (Atomic.get pool.stop) do
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
          && nothing_to_do pool
        then Condition.signal pool.working;
        Mutex.unlock (fst pool.work)
      done
    with Exit ->
      ignore (Atomic.fetch_and_add pool.domains_counter (-1));
      Condition.signal pool.working;
      Mutex.unlock (fst pool.work)

  let push pool ~fn prm =
    Mutex.lock (fst pool.work);
    L.push (Shared.Task (prm, fn)) pool.glist;
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
    L.drop pool.glist;
    (* XXX(dinosaure): [glist] should be empty. *)
    Atomic.set pool.stop true;
    Condition.broadcast (snd pool.work);
    Mutex.unlock (fst pool.work);
    wait pool

  let domain ~events ?(g = Random.State.make_self_init ()) idx =
    {
      g
    ; llist= L.make g
    ; blist= L.make g
    ; uid= Did.gen ()
    ; events= events (succ idx)
    }

  let make ?(g = Random.State.make_self_init ())
      ?(domains = max 0 (Domain.recommended_domain_count () - 1)) ~events go =
    let domains = List.init domains (domain ~events ~g:(Random.State.copy g)) in
    let pool =
      {
        glist= L.make g
      ; clist= L.make g
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
  let resources = Tq.make () in
  let f (Own.Resource { active; _ } as resource) =
    Tq.enqueue resources (Own.copy resource);
    Atomic.set active false
  in
  List.iter f ress;
  let prm =
    {
      Prm.uid= Id.gen ()
    ; domain= Atomic.make (Prm.Chosen domain.uid)
    ; state= Atomic.make (Prm.Pending : _ Prm.pstate)
    ; ty= Ty.Task
    ; children= Tq.make ()
    ; resources
    ; parent= Some (Prm.Prm parent)
    }
  in
  Option.iter (L.push prm) orphans;
  Tq.enqueue parent.Prm.children (Prm.Prm prm);
  L.push (Local.Task (prm, fn)) domain.llist;
  Effect.Deep.continue k prm

let do_parallel_spawn pool ress ?orphans ~parent fn k =
  if Array.length pool.domains <= 0 then
    Effect.Deep.discontinue k No_domain_available
  else
    let resources = Tq.make () in
    let f (Own.Resource { active; _ } as resource) =
      Tq.enqueue resources (Own.copy resource);
      Atomic.set active false
    in
    List.iter f ress;
    let prm =
      {
        Prm.uid= Id.gen ()
      ; domain= Atomic.make Prm.On_hold
      ; state= Atomic.make (Prm.Pending : _ Prm.pstate)
      ; ty= Ty.Task
      ; children= Tq.make ()
      ; resources
      ; parent= Some (Prm.Prm parent)
      }
    in
    Option.iter (L.push prm) orphans;
    Tq.enqueue parent.Prm.children (Prm.Prm prm);
    Pool.push pool ~fn prm;
    Effect.Deep.continue k prm

let is_a_child_of ~parent prm =
  let res = ref false in
  let f (Prm.Prm child) = res := !res || child.uid = prm.Prm.uid in
  Tq.iter ~f parent.Prm.children;
  !res

let rec until_is_resolved pool domain go ~parent prm k =
  match (prm.Prm.ty, Atomic.get prm.Prm.state) with
  | _, Prm.Consumed (Error ((Still_has_children | Not_a_child) as exn))
  | _, Prm.Failed ((Still_has_children | Not_a_child) as exn) ->
      Effect.Deep.discontinue k exn
  | _, Prm.Consumed res -> Effect.Deep.continue k res
  | _, Prm.(Resolved _ | Failed _) ->
      Effect.Deep.continue k (Prm.to_consumed prm)
  | Ty.Block return, Prm.Pending ->
      (* XXX(dinosaure): actually [do_suspend] do this. *)
      L.push (Local.Block (prm, return, k)) domain.blist
  | Ty.Task, Prm.Pending -> (
      match Atomic.get prm.Prm.domain with
      | Prm.On_hold -> until_is_resolved pool domain go ~parent prm k
      | Prm.Chosen uid when domain.uid = uid ->
          Run.run_local pool domain go;
          until_is_resolved pool domain go ~parent prm k
      | Prm.Chosen _ ->
          await_any_domains domain pool;
          until_is_resolved pool domain go ~parent prm k)

let do_await pool domain go ~parent prm k =
  if not (is_a_child_of ~parent prm) then Effect.Deep.discontinue k Not_a_child;
  until_is_resolved pool domain go ~parent prm k

let do_suspend domain ~parent prm k =
  if not (is_a_child_of ~parent prm) then Effect.Deep.discontinue k Not_a_child;
  if Prm.is_pending prm then begin
    (* XXX(dinosaure): [do_suspend] should be run by a [Suspend] which only
       accepts promise with [Ty.Block]. *)
    let[@warning "-8"] (Ty.Block return) = prm.Prm.ty in
    L.push (Local.Block (prm, return, k)) domain.blist
  end

let do_yield pool domain go k =
  Run.run_local pool domain go;
  Effect.Deep.continue k ()

let rec until_is_cancelled pool domain go prm =
  match (prm.Prm.ty, Atomic.get prm.Prm.state) with
  | _, Prm.Consumed _ -> ()
  | _, Prm.(Resolved _ | Failed _) -> until_is_cancelled pool domain go prm
  | _, Prm.Pending -> (
      match Atomic.get prm.Prm.domain with
      | Prm.On_hold -> until_is_cancelled pool domain go prm
      | Prm.Chosen uid when domain.uid = uid ->
          Run.run_local pool domain go;
          until_is_cancelled pool domain go prm
      | Prm.Chosen _ ->
          await_any_domains domain pool;
          until_is_cancelled pool domain go prm)

let rec cancel : type a. pool -> domain -> go -> a Prm.t -> unit =
 fun pool domain go prm ->
  Tq.iter ~f:(fun (Prm.Prm prm) -> cancel pool domain go prm) prm.children;
  Mutex.lock (fst pool.work);
  begin
    match (Atomic.get prm.Prm.state, Atomic.get prm.Prm.domain) with
    | Prm.Consumed _, _ -> ()
    | _, Prm.On_hold -> Pool.remove_prm_from_pool pool prm
    | _, Prm.Chosen uid when uid = domain.uid ->
        (* XXX(dinosaure): this code is safe because we are surrounded by a
           lock and the task can only be executed in the current domain, which
           is already busy cancelling this task. *)
        let f0 node =
          let (Prm.Prm prm') = Local.prm (L.data node) in
          if prm'.Prm.uid = prm.Prm.uid then L.remove node
        in
        let f1 node =
          let (Local.Block (prm', _, _)) = L.data node in
          if prm'.Prm.uid = prm.Prm.uid then L.remove node
        in
        L.iter_on ~f:f0 domain.llist;
        L.iter_on ~f:f1 domain.blist;
        Prm.to_consumed_with prm (Error Prm.Cancelled)
    | _, Prm.Chosen uid ->
        (* XXX(dinosaure): this code is like [Pool.cancel] but we are already
           surrounded by the lock of [fst pool.work]. *)
        L.push (Shared.Cancel prm) pool.clist;
        interrupt_domain ~uid pool;
        Condition.broadcast (snd pool.work)
  end;
  Mutex.unlock (fst pool.work);
  until_is_cancelled pool domain go prm

let do_cancel pool domain go ~parent prm k =
  if not (is_a_child_of ~parent prm) then Effect.Deep.discontinue k Not_a_child;
  cancel pool domain go prm;
  Effect.Deep.continue k ()

let do_syscall_spawn domain ress ~parent ~return k =
  let resources = Tq.make () in
  let f (Own.Resource { active; _ } as resource) =
    Tq.enqueue resources (Own.copy resource);
    Atomic.set active false
  in
  List.iter f ress;
  let prm =
    {
      Prm.uid= Id.gen ()
    ; domain= Atomic.make (Prm.Chosen domain.uid)
    ; state= Atomic.make (Prm.Pending : _ Prm.pstate)
    ; ty= Ty.Block return
    ; children= Tq.make ()
    ; resources= Tq.make ()
    ; parent= Some (Prm.Prm parent)
    }
  in
  if Prm.is_pending parent then (
    Tq.enqueue parent.Prm.children (Prm.Prm prm);
    Effect.Deep.continue k prm)
  else Effect.Deep.discontinue k Prm.Cancelled

let discontinue_if_invalid k prm =
  match Atomic.get prm.Prm.state with
  | Consumed (Error ((Still_has_children | Not_a_child) as exn))
  | Failed ((Still_has_children | Not_a_child) as exn) ->
      Effect.Deep.discontinue k exn;
      true
  | _ -> false

let do_await_one ~and_cancel pool domain go ~parent prms k =
  if not (List.for_all (is_a_child_of ~parent) prms) then
    Effect.Deep.discontinue k Not_a_child;
  let rec until () =
    let unresolved, resolved = List.partition Prm.is_pending prms in
    match resolved with
    | [] ->
        Run.run_local pool domain go;
        await_any_domains domain pool;
        until ()
    | resolved when List.exists (discontinue_if_invalid k) resolved ->
        if and_cancel then List.iter (cancel pool domain go) unresolved
    | resolved when and_cancel ->
        let resolved = List.map Prm.to_consumed resolved in
        let len = List.length resolved in
        let idx = Random.State.int domain.g len in
        let res = List.nth resolved idx in
        List.iter (fun prm -> cancel pool domain go prm) unresolved;
        Effect.Deep.continue k res
    | resolved ->
        let len = List.length resolved in
        let idx = Random.State.int domain.g len in
        let prm = List.nth resolved idx in
        Effect.Deep.continue k (Prm.to_consumed prm)
  in
  until ()

let do_await_all pool domain go ~parent prms k =
  if not (List.for_all (is_a_child_of ~parent) prms) then
    Effect.Deep.discontinue k Not_a_child;
  let rec until = function
    | [] -> Effect.Deep.continue k (List.map Prm.to_consumed prms)
    | unresolved ->
        Run.run_local pool domain go;
        await_any_domains domain pool;
        let unresolved, resolved = List.partition Prm.is_pending unresolved in
        if not (List.exists (discontinue_if_invalid k) resolved) then
          until unresolved
  in
  until prms

let do_own prm (Own.Resource { uid; _ } as resource) k =
  let exists = ref false in
  let f (Own.Resource { uid= uid'; _ }) = exists := !exists || uid = uid' in
  Tq.iter ~f prm.Prm.resources;
  if not !exists then Tq.enqueue prm.Prm.resources resource;
  Effect.Deep.continue k resource

let do_disown prm (Own.Resource { uid; _ }) k =
  let f (Own.Resource { uid= uid'; active; _ }) =
    if uid = uid' then Atomic.set active false
  in
  Tq.iter ~f prm.Prm.resources;
  Effect.Deep.continue k ()

let collect_pending_children prm =
  let res = ref [] in
  let f (Prm.Prm prm) =
    if not (Prm.is_consumed prm) then res := Prm.Prm prm :: !res
  in
  Tq.iter ~f prm.Prm.children;
  !res

let collect_pending_resources prm =
  let res = ref [] in
  let f (Own.Resource { active; _ } as resource) =
    if Atomic.get active then res := resource :: !res
  in
  Tq.iter ~f prm.Prm.resources;
  !res

let run ?(g = Random.State.make_self_init ()) ?domains ~events fn =
  let rec go : type a. pool -> domain -> a Prm.t -> (unit -> a) -> unit =
   fun pool domain prm fn ->
    let retc value =
      match (collect_pending_children prm, collect_pending_resources prm) with
      | [], [] -> Prm.to_resolved prm value
      | prms, [] ->
          List.iter (fun (Prm.Prm prm) -> cancel pool domain { go } prm) prms;
          Prm.to_consumed_with prm (Error Still_has_children)
      | _, resources ->
          List.iter (fun resource -> Own.finalise resource) resources;
          Prm.to_consumed_with prm (Error Resource_leak)
    in
    let exnc exn =
      Tq.iter
        ~f:(fun (Prm.Prm prm) -> cancel pool domain { go } prm)
        prm.Prm.children;
      Tq.iter ~f:(fun resource -> Own.finalise resource) prm.Prm.resources;
      Prm.to_failed prm exn;
      match exn with
      | (Still_has_children | Not_a_child | Resource_leak) as exn -> raise exn
      | _ -> ()
    in
    let effc :
        type a.
        a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option =
     fun eff ->
      match (Atomic.get prm.Prm.state, eff) with
      | Prm.(Failed exn | Consumed (Error exn)), _ ->
          Some (fun k -> Effect.Deep.discontinue k exn)
      | _, Did.Did -> Some (fun k -> Effect.Deep.continue k domain.uid)
      | _, Prm.Local_spawn (orphans, ress, fn) ->
          Some (do_local_spawn domain ress ?orphans ~parent:prm fn)
      | _, Prm.Parallel_spawn (orphans, ress, fn) ->
          Some (do_parallel_spawn pool ress ?orphans ~parent:prm fn)
      | _, Prm.Syscall_spawn (ress, return) ->
          Some (do_syscall_spawn domain ress ~parent:prm ~return)
      | _, Prm.Await prm' -> Some (do_await pool domain { go } ~parent:prm prm')
      | _, Prm.Suspend prm' -> Some (do_suspend domain ~parent:prm prm')
      | _, Prm.Await_one (and_cancel, prms) ->
          Some (do_await_one ~and_cancel pool domain { go } ~parent:prm prms)
      | _, Prm.Await_all prms ->
          Some (do_await_all pool domain { go } ~parent:prm prms)
      | _, Yield -> Some (do_yield pool domain { go })
      | _, Prm.Cancel prm' ->
          Some (do_cancel pool domain { go } ~parent:prm prm')
      | _, Own.Own resource -> Some (do_own prm resource)
      | _, Own.Disown resource -> Some (do_disown prm resource)
      | _ -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  let pool, domains =
    Pool.make ~g:(Random.State.copy g) ?domains ~events { go }
  in
  Did.reset ();
  let dom0 =
    {
      g= Random.State.copy g
    ; llist= L.make g
    ; blist= L.make g
    ; uid= Did.gen ()
    ; events= events 0
    }
  in
  let prm0 =
    {
      Prm.uid= Id.gen ()
    ; domain= Atomic.make (Prm.Chosen dom0.uid)
    ; state= Atomic.make (Prm.Pending : _ Prm.pstate)
    ; ty= Task
    ; children= Tq.make ()
    ; resources= Tq.make ()
    ; parent= None
    }
  in
  L.push (Local.Task (prm0, fn)) dom0.llist;
  Run.run_local pool dom0 { go };
  Pool.kill pool;
  List.iter Domain.join domains;
  Prm.to_result_exn prm0

let always_no_events _ = { interrupt= Fun.const (); select= Fun.const [] }
let task prm fn = Task (prm, fn)

let run ?g ?domains ?(events = always_no_events) fn =
  or_raise (run ?g ?domains ~events fn)
