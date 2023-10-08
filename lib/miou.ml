let failwith fmt = Format.kasprintf failwith fmt
let str fmt = Format.kasprintf Fun.id fmt

external apply : ('a -> 'b) -> 'a -> 'b = "%apply"
external reraise : exn -> unit = "%reraise"

module Queue = Queue
module State = State

(** Uid modules *)

module Domain_uid = struct
  type t = int

  let null = -1
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int
  let to_int x = x

  let gen, reset =
    let value = Atomic.make (null + 1) in
    let gen () = Atomic.fetch_and_add value 1 in
    let reset () = Atomic.set value (null + 1) in
    (gen, reset)
end

module Promise_uid = struct
  type t = int

  let null = -1
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int

  let gen =
    let value = Atomic.make (null + 1) in
    fun () -> Atomic.fetch_and_add value 1
end

module Syscall_uid = struct
  type t = int

  let null = -1
  let pp = Format.pp_print_int

  let gen =
    let value = Atomic.make (null + 1) in
    fun () -> Atomic.fetch_and_add value 1
end

module Resource_uid = struct
  type t = int

  let null = -1
  let equal = Int.equal
  let pp = Format.pp_print_int

  let gen =
    let value = Atomic.make (null + 1) in
    fun () -> Atomic.fetch_and_add value 1
end

module Backoff = struct
  (* Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
     Copyright (c) 2016, KC Sivaramakrishnan <kc@kcsrk.info>
     Copyright (c) 2021, Sudha Parimala <sudharg247@gmail.com>
     Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>

     Under ISC license. *)

  let bits = 5
  let mask = 1 lsl bits
  let max_wait_log = 30
  let single_mask = Bool.to_int (Domain.recommended_domain_count () = 1) - 1

  let make ?(lower_wait_log = 4) ?(upper_wait_log = 17) () =
    if
      not
        (0 <= lower_wait_log
        && lower_wait_log <= upper_wait_log
        && upper_wait_log <= max_wait_log)
    then invalid_arg "Backoff.make";
    (upper_wait_log lsl (bits * 2))
    lor (lower_wait_log lsl bits)
    lor lower_wait_log

  let get_upper_wait_log t = t lsr (bits * 2)
  let get_lower_wait_log t = (t lsr bits) land mask
  let get_wait_log t = t land mask

  let[@warning "-32"] reset t =
    let lower_wait_log = get_lower_wait_log t in
    t land lnot mask lor lower_wait_log

  let once t =
    let wait_log = get_wait_log t in
    let wait_mask = (1 lsl wait_log) - 1 in
    let v = Random.bits () land wait_mask land single_mask in
    for _ = 0 to v do
      Domain.cpu_relax ()
    done;
    let upper_wait_log = get_upper_wait_log t in
    let next_wait_log = Int.min upper_wait_log (wait_log + 1) in
    t lxor wait_log lor next_wait_log

  let default = make ()
end

(** Exceptions *)

exception Cancelled
exception Not_a_child
exception Still_has_children
exception Not_owner
exception Resource_leak
exception No_domain_available

(** Resource *)

type resource =
  | Resource : {
        uid: Resource_uid.t
      ; value: 'a
      ; finaliser: 'a -> unit
    }
      -> resource

module Resource = struct
  module Uid = Resource_uid

  let make ~finaliser value =
    let uid = Uid.gen () in
    Resource { uid; value; finaliser }
end

(** Promise *)

type 'a state =
  | Pending
  | Resolved of 'a
  | Failed of exn
  | Consumed of ('a, exn) result

type 'a t = {
    uid: Promise_uid.t
  ; runner: Domain_uid.t
  ; state: 'a state Atomic.t
  ; parent: pack option
  ; children: pack Queue.t
  ; resources: resource Sequence.t
}

and pack = Pack : 'a t -> pack

module Promise = struct
  module Uid = Promise_uid

  let equal prm0 prm1 = Uid.equal prm0.uid prm1.uid

  let pp ppf { uid; runner; resources; _ } =
    Format.fprintf ppf "[%a:%a](%d)" Domain_uid.pp runner Uid.pp uid
      (Sequence.length resources)

  let rec transition ?(backoff = Backoff.default) prm value =
    match (Atomic.get prm.state, value) with
    | Failed Cancelled, _ | Consumed (Error Cancelled), _ -> ()
    | (Pending as state), Ok v ->
        let set = Atomic.compare_and_set prm.state state (Resolved v) in
        if not set then transition ~backoff:(Backoff.once backoff) prm value
    | (Pending as state), Error e ->
        let set = Atomic.compare_and_set prm.state state (Failed e) in
        if not set then transition ~backoff:(Backoff.once backoff) prm value
    | _ -> failwith "Promise.transition: Invalid transition"

  let rec consume ?(backoff = Backoff.default) prm =
    match Atomic.get prm.state with
    | Consumed _ -> ()
    | Resolved v as state ->
        let set = Atomic.compare_and_set prm.state state (Consumed (Ok v)) in
        if not set then consume ~backoff:(Backoff.once backoff) prm
    | Failed e as state ->
        let set = Atomic.compare_and_set prm.state state (Consumed (Error e)) in
        if not set then consume ~backoff:(Backoff.once backoff) prm
    | _ -> failwith "Promise.consume: Invalid transition"

  let rec cancel ?(backoff = Backoff.default) prm =
    match Atomic.get prm.state with
    | (Pending | Resolved _ | Failed _) as state ->
        let consumed = Consumed (Error Cancelled) in
        let set = Atomic.compare_and_set prm.state state consumed in
        if not set then cancel ~backoff:(Backoff.once backoff) prm
    | Consumed _ -> ()

  let is_cancelled prm =
    match Atomic.get prm.state with
    | Consumed (Error Cancelled) | Failed Cancelled -> true
    | _ -> false

  let is_pending prm =
    match Atomic.get prm.state with Pending -> true | _ -> false

  let is_consumed prm =
    match Atomic.get prm.state with Consumed _ -> true | _ -> false

  let is_resolved prm =
    match Atomic.get prm.state with Resolved _ -> true | _ -> false

  let make :
      type a.
         resources:resource list
      -> runner:Domain_uid.t
      -> ?parent:a t
      -> unit
      -> _ t =
   fun ~resources:ress ~runner ?parent () ->
    (* TODO(dinosaure): replace [Random.State.make_self_init] or delete it. *)
    let resources = Sequence.create (Random.State.make_self_init ()) in
    List.iter (fun res -> Sequence.push res resources) ress;
    let parent = Option.map (fun parent -> Pack parent) parent in
    {
      uid= Uid.gen ()
    ; runner
    ; state= Atomic.make Pending
    ; parent
    ; children= Queue.create ()
    ; resources
    }

  let pack : type a. a t -> pack = fun prm -> Pack prm

  let to_result prm =
    match Atomic.get prm.state with
    | Pending -> failwith "Promise.to_result"
    | Resolved value -> Ok value
    | Failed exn -> Error exn
    | Consumed value -> value

  let is_a_children ~parent prm =
    let yes = ref false in
    Queue.iter
      ~f:(fun (Pack prm') -> yes := !yes || Promise_uid.equal prm.uid prm'.uid)
      parent.children;
    !yes

  let children_terminated prm =
    let result = ref false in
    let check (Pack prm') =
      match Atomic.get prm'.state with
      | Consumed _ | Failed Cancelled -> ()
      | _ -> result := true
    in
    Queue.iter ~f:check prm.children;
    not !result

  let uid { uid; _ } = uid
end

(** Effects *)

[@@@warning "-30"]

type ty = Concurrent | Parallel of Domain_uid.t
type _ Effect.t += Domain_uid : Domain_uid.t Effect.t
type _ Effect.t += Domain_count : int Effect.t
type _ Effect.t += Spawn : ty * resource list * (unit -> 'a) -> 'a t Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Cancel : 'a t -> unit Effect.t
type _ Effect.t += Domains : Domain_uid.t list Effect.t
type _ Effect.t += Random : Random.State.t Effect.t
type _ Effect.t += Self : (Promise.Uid.t * Domain_uid.t * int) Effect.t

type _ Effect.t +=
  | Await : 'a t -> ('a, exn) result Effect.t
  | Await_all : 'a t list -> ('a, exn) result list Effect.t
  | Await_one : bool * 'a t list -> ('a, exn) result Effect.t
  | Both : 'a t * 'b t -> (('a, exn) result * ('b, exn) result) Effect.t

type 'a ownership =
  | Check : resource -> unit ownership
  | Own : resource -> resource ownership
  | Disown : resource -> unit ownership
  | Transfer : resource -> unit ownership

type _ Effect.t += Ownership : 'a ownership -> 'a Effect.t

(** Syscalls, Domain & Pool. *)

type task =
  | Arrived : 'a t * (unit -> 'a) -> task
  | Suspended : 'a t * 'a State.t -> task
  | Cancelled : 'a t -> task
  | Tick : task

and system =
  | System_call_suspended :
      Syscall_uid.t * (unit -> 'a) * 'b t * ('a, 'b) State.continuation
      -> system

and 'a syscall = Syscall : Syscall_uid.t * (unit -> 'a) -> 'a syscall

and domain = {
    g: Random.State.t
  ; tasks: (int * task) Heapq.t
  ; system_tasks: (Syscall_uid.t, system) Hashtbl.t
  ; uid: Domain_uid.t
  ; events: events
  ; quanta: int
  ; tick: int Atomic.t
}

and elt = Task : 'a t * (unit -> 'a) -> elt | Cancel : 'a t -> elt

and pool = {
    tasks: elt Sequence.t
  ; mutex: Mutex.t
  ; condition_pending_work: Condition.t
  ; condition_all_idle: Condition.t
  ; domains: domain list
  ; mutable stop: bool
  ; mutable fail: bool
  ; mutable working_counter: int
  ; mutable domains_counter: int
}

and continue = Continue : Syscall_uid.t * (unit -> unit) -> continue
and events = { select: unit -> continue list; interrupt: unit -> unit }
and handler = { handler: 'u 'v. ('u -> 'v) -> 'u -> 'v } [@@unboxed]
and uid = Syscall_uid.t

type _ Effect.t += Syscall : (unit -> 'a) -> 'a syscall Effect.t
type _ Effect.t += Suspend : 'a syscall -> 'a Effect.t
type _ Effect.t += Syscall_exists : Syscall_uid.t -> bool Effect.t

let dummy_events = { select= Fun.const []; interrupt= ignore }
let dummy_handler = { handler= apply }

module Domain = struct
  module Uid = Domain_uid

  module Task = struct
    let to_int = function
      | Cancelled _ -> 0
      | Arrived _ -> 1
      | Suspended _ -> 2
      | Tick -> 3

    let compare a b = to_int a - to_int b
  end

  let make ?(quanta = 1) ~g events =
    let uid = Uid.gen () in
    let compare (tick0, a) (tick1, b) =
      let order = Task.compare a b in
      if order = 0 then Int.compare tick0 tick1 else order
    in
    {
      g= Random.State.copy g
    ; uid
    ; tasks= Heapq.create ~compare ~dummy:(0, Tick) 0x100
    ; system_tasks= Hashtbl.create 0x10
    ; events= events uid
    ; quanta
    ; tick= Atomic.make 0
    }

  let self () = Effect.perform Domain_uid
  let count () = Effect.perform Domain_count

  (* It tries to aggregate all the events/[task] that involve the given promise
     with the unique ID [uid]. This function is used for cancellation. It is
     noted that we **do not** clean up events here. We only aggregate them and
     filter only suspensions or arrivals.

     We can long for the cancellation of a promise and have several [Cancelled]
     in our heap. However, there should only be one suspension or (exclusive)
     one [Arrived]. For more precision, see the only [assert false] in our code.

     Regarding the fact that we don't really clean up, our function that handles
     these events only handles tasks that have not been cancelled. This means
     that a [Heapq.pop_minimum] will consume those junk tasks and, if the
     cancellation went well, nothing should happen. *)
  let clean_promise ~uid domain =
    let tasks = ref [] in
    let f (_, task) =
      match task with
      | Arrived (prm, _) ->
          if Promise_uid.equal prm.uid uid then tasks := task :: !tasks
      | Suspended (prm, _) ->
          if Promise_uid.equal prm.uid uid then tasks := task :: !tasks
      | Cancelled prm ->
          if Promise_uid.equal prm.uid uid then tasks := task :: !tasks
      | Tick -> ()
    in
    Heapq.iter f domain.tasks;
    List.filter_map
      (function (Arrived _ | Suspended _) as task -> Some task | _ -> None)
      !tasks

  let add_into_pool pool task =
    Mutex.lock pool.mutex;
    begin
      match task with
      | Task _ -> Sequence.add_r task pool.tasks
      | Cancel _ -> Sequence.add_l task pool.tasks
    end;
    Condition.broadcast pool.condition_pending_work;
    Mutex.unlock pool.mutex

  let add_task domain task =
    let tick = Atomic.fetch_and_add domain.tick 1 in
    Heapq.add domain.tasks (tick, task)

  let task_is_cancelled = function
    | Arrived (prm, _) -> Promise.is_cancelled prm
    | Suspended (prm, _) -> Promise.is_cancelled prm
    | Cancelled prm -> Promise.is_cancelled prm
    | Tick -> false

  let pack_is_cancelled (Pack prm) = Promise.is_cancelled prm

  let suspend_system_call domain (Syscall (uid, fn) : _ syscall) prm k =
    match Hashtbl.find domain.system_tasks uid with
    | System_call_suspended _ ->
        failwith "%a has already been suspended" Promise.pp prm
    | exception Not_found ->
        Hashtbl.replace domain.system_tasks uid
          (System_call_suspended (uid, fn, prm, k))

  (* The interruption makes it possible to re-synchronize a domain even if the
     latter has fallen in the case of the management of system events
     [domain.events.select ()] - which is, currently, the only blocking
     operation of our scheduler. It occurs especially when you cancel a task
     that belongs to the said domain. *)
  let interrupt ~domain =
    Logs.debug (fun m -> m "interrupts domain [%a]" Domain_uid.pp domain.uid);
    domain.events.interrupt ()

  (* This function is properly the cancellation of a task (whether it works in
     the current domain or not). If it belongs to another domain, we [interrupt]
     said domain and add a cancellation task to our [pool]. Otherwise, we just
     need to add the cancellation task to our heap and reschedule.

     Note that cancellation tasks take priority. *)
  let terminate pool domain (Pack prm) =
    if not (Domain_uid.equal prm.runner domain.uid) then (
      let domain' =
        List.find
          (fun domain -> Domain_uid.equal prm.runner domain.uid)
          pool.domains
      in
      add_into_pool pool (Cancel prm);
      Logs.debug (fun m ->
          m "[%a] signals the cancellation of %a to [%a]" Domain_uid.pp
            domain.uid Promise.pp prm Domain_uid.pp domain'.uid);
      interrupt ~domain:domain')
    else begin
      Hashtbl.filter_map_inplace
        (fun _ (System_call_suspended (_, _, prm', _) as v) ->
          if Promise.equal prm prm' then None else Some v)
        domain.system_tasks;
      add_task domain (Cancelled prm)
    end

  (* XXX(dinosaure): Promises can end with "uncatchable" exceptions. This means
     that these exceptions terminate the program in any case. The user cannot
     catch them because they are not exposed. Such a situation arises only if
     the user violates one of our rules. The error is therefore not linked to
     the system, to the scheduler, but to the way the user uses our scheduler.

     Here, we check if a promise of a given list has such an error and then
     wildly discontinue it. Even the best of anarchists sitting on a Tsar's
     throne would become the worst of despot. *)
  let unreachable_exception prms =
    let exception Unreachable_exception of exn in
    try
      List.iter
        (fun prm ->
          match Atomic.get prm.state with
          | Pending | Resolved _ | Consumed (Ok _) -> ()
          | Consumed (Error exn) | Failed exn -> (
              match exn with
              | Not_a_child | Still_has_children ->
                  raise_notrace (Unreachable_exception exn)
              | _ -> ()))
        prms;
      None
    with Unreachable_exception exn -> Some exn

  type 'a Effect.t += Await_cancellation_of : pack list * 'a -> 'a Effect.t

  (* This effect is a special effect that can only exist internally in our
     scheduler. The only one capable of producing this effect is our scheduler -
     for more details, see the [State] module and the [Cont] step. This effect
     allows us to implement cancellation asynchronously. That is to say that it
     is a task like all the others which will be executed concurrently with the
     others - this saves us above all a mechanism of synchronization on what the
     cancellation implies.

     Thus, on the suspension point where the [cancel] was done, the latter will
     be released when the task and its children have been successfully
     completed/cancelled. However, other tasks (if they exist) will be able to
     be executed concurrently. This allows us to reaffirm the availability to
     perform all our tasks regardless of a cancellation. *)
  let await_cancellation_of prms ~and_return =
    State.Cont (Await_cancellation_of (prms, and_return))

  (* A subtlety prevails here. Normally only the domain in charge of the task
     can modify the promise. But in this case, it is the parent (having launched
     the task) that modifies the promise (note that the task can run into [dom1]
     and the parent runs into [dom0] due to a [Miou.call]). In reality, in a
     [Resolved]/[Failed] state, the task has ended and will no longer have to
     change its state. In this situation, the only one who can interact with the
     promise (and its state) therefore remains the parent who can "consume"
     (await) or cancel the task.

     In other words, if the parent observes that the promise is
     [Resolved]/[Failed], it can safely obtain exclusive ownership of it and
     change its state.

     Attention, the check about the children of a task is done in the [handle]
     function. The [Resolved]/[Failed] state only appears if all children of the
     promise have completed successfully. The same is true in the case of an
     exception where the domain will modify the state only if all the children
     have been canceled. For more information, see the [handle] function. *)
  let await prm =
    match unreachable_exception [ prm ] with
    | Some exn -> State.Fail exn
    | None -> (
        match Atomic.get prm.state with
        | Consumed value -> State.Send value
        | Resolved value -> Promise.consume prm; State.Send (Ok value)
        | Failed exn -> Promise.consume prm; State.Send (Error exn)
        | Pending -> State.Intr)

  let await_all prms =
    let pending, _ = List.partition Promise.is_pending prms in
    match pending with
    | [] -> (
        List.iter Promise.consume prms;
        let results =
          List.map
            (fun prm ->
              match Atomic.get prm.state with
              | Consumed value -> value
              | _ -> assert false)
            prms
        in
        match unreachable_exception prms with
        | Some exn -> State.Fail exn
        | None -> State.Send results)
    | _pending -> State.Intr

  let await_one ~and_cancel pool domain prms =
    match List.partition Promise.is_pending prms with
    | _pending, [] -> State.Intr
    | _, terminated when and_cancel -> (
        let terminated =
          match List.partition Promise.is_resolved terminated with
          | [], _ -> terminated
          | terminated, _ -> terminated
        in
        let len = List.length terminated in
        let prm = List.nth terminated (Random.State.int domain.g len) in
        let to_cancel = List.filter (Fun.negate (Promise.equal prm)) prms in
        let to_cancel = List.map Promise.pack to_cancel in
        Logs.debug (fun m ->
            m "[%a] cancels all pending tasks" Domain_uid.pp domain.uid);
        List.iter (terminate pool domain) to_cancel;
        Promise.consume prm;
        match unreachable_exception prms with
        | Some exn -> State.Fail exn
        | None ->
            let and_return = Promise.to_result prm in
            await_cancellation_of ~and_return to_cancel)
    | _, terminated -> (
        let terminated =
          match List.partition Promise.is_resolved terminated with
          | [], _ -> terminated
          | terminated, _ -> terminated
        in
        let len = List.length terminated in
        let prm = List.nth terminated (Random.State.int domain.g len) in
        match unreachable_exception prms with
        | Some exn -> State.Fail exn
        | None ->
            Promise.consume prm;
            State.Send (Promise.to_result prm))

  let both prm0 prm1 =
    if Promise.is_pending prm0 = false && Promise.is_pending prm1 = false then begin
      Logs.debug (fun m ->
          m "consumes %a and %a" Promise.pp prm0 Promise.pp prm1);
      Promise.consume prm0;
      Promise.consume prm1;
      let unreachable_exception =
        match
          (unreachable_exception [ prm0 ], unreachable_exception [ prm1 ])
        with
        | Some exn, _ | _, Some exn -> Some exn
        | _ -> None
      in
      match unreachable_exception with
      | Some exn -> State.Fail exn
      | None ->
          let[@warning "-8"] (Consumed v0) = Atomic.get prm0.state in
          let[@warning "-8"] (Consumed v1) = Atomic.get prm1.state in
          State.Send (v0, v1)
    end
    else State.Intr

  let error_syscall_exists =
    Invalid_argument "Miou.is_pending can be used only in events.select"

  let error_already_owned prm (Resource { uid; _ }) =
    Invalid_argument
      (str "Miou.own: the resource [%a] was already owned by %a" Domain_uid.pp
         uid Promise.pp prm)

  let error_impossible_to_transfer prm =
    Invalid_argument
      (str "Miou.Ownership.transfer: impossible to transfer from %a" Promise.pp
         prm)

  let own_it prm (Resource { uid; _ }) =
    let own_it = ref false in
    let f (Resource { uid= uid'; _ }) =
      own_it := !own_it || Resource_uid.equal uid uid'
    in
    Sequence.iter ~f prm.resources;
    !own_it

  let ownership :
      type a b c. c t -> (a State.step -> b State.t) -> a ownership -> b State.t
      =
   fun prm k -> function
    | Check (Resource { uid; _ } as res) ->
        Logs.debug (fun m ->
            m "check if %a owns [%a]" Promise.pp prm Resource_uid.pp uid);
        if own_it prm res = false then k (State.Fail Not_owner)
        else k (State.Send ())
    | Own (Resource { uid; _ } as res) ->
        Logs.debug (fun m ->
            m "%a owns [%a]" Promise.pp prm Resource_uid.pp uid);
        if own_it prm res = false then begin
          Sequence.push res prm.resources;
          k (State.Send res)
        end
        else k (State.Fail (error_already_owned prm res))
    | Disown (Resource { uid; _ } as res) ->
        Logs.debug (fun m ->
            m "%a disown [%a] (own it? %b)" Promise.pp prm Resource_uid.pp uid
              (own_it prm res));
        if own_it prm res then begin
          let to_delete = ref None in
          let f node =
            let (Resource { uid= uid'; _ }) = Sequence.data node in
            if uid = uid' then to_delete := Some node
          in
          Sequence.iter_node ~f prm.resources;
          let[@warning "-8"] (Some node) = !to_delete in
          Sequence.remove node; k (State.Send ())
        end
        else k (State.Fail Not_owner)
    | Transfer (Resource { uid; _ } as res) ->
        if own_it prm res then begin
          match prm.parent with
          | None -> k (State.Fail (error_impossible_to_transfer prm))
          | Some (Pack prm') ->
              let to_transmit = ref None in
              let f node =
                let (Resource { uid= uid'; _ }) = Sequence.data node in
                if uid = uid' then to_transmit := Some node
              in
              Sequence.iter_node ~f prm.resources;
              let[@warning "-8"] (Some node) = !to_transmit in
              Sequence.push (Sequence.data node) prm'.resources;
              Sequence.remove node;
              k (State.Send ())
        end
        else k (State.Fail Not_owner)

  let perform pool domain (Pack current) =
    let perform : type a b. (a, b) State.k =
     fun k -> function
      | Domain_uid -> k (State.Send domain.uid)
      | Domain_count -> k (State.Send (List.length pool.domains))
      | Domains ->
          let uids = List.map (fun { uid; _ } -> uid) pool.domains in
          k (State.Send uids)
      | Random -> k (State.Send domain.g)
      | Self ->
          let prm =
            (current.uid, current.runner, Sequence.length current.resources)
          in
          k (State.Send prm)
      | Spawn (Concurrent, resources, fn) ->
          let runner = domain.uid in
          let prm = Promise.make ~resources ~runner ~parent:current () in
          Queue.enqueue current.children (Promise.pack prm);
          add_task domain (Arrived (prm, fn));
          k (State.Send prm)
      | Spawn (Parallel runner, resources, fn)
        when Domain_uid.equal runner domain.uid ->
          let prm = Promise.make ~resources ~runner ~parent:current () in
          Logs.debug (fun m ->
              m "[%a] spawn a new task %a" Domain_uid.pp domain.uid Promise.pp
                prm);
          Queue.enqueue current.children (Promise.pack prm);
          add_task domain (Arrived (prm, fn));
          k (State.Send prm)
      | Spawn (Parallel runner, resources, fn) ->
          let prm = Promise.make ~resources ~runner ~parent:current () in
          Logs.debug (fun m ->
              m "[%a] spawn a new task %a" Domain_uid.pp domain.uid Promise.pp
                prm);
          Queue.enqueue current.children (Promise.pack prm);
          add_into_pool pool (Task (prm, fn));
          k (State.Send prm)
      | Syscall fn ->
          let uid = Syscall_uid.gen () in
          Logs.debug (fun m ->
              m "[%a] creates a suspension point [%a]" Domain_uid.pp domain.uid
                Syscall_uid.pp uid);
          k (State.Send (Syscall (uid, fn)))
      | Syscall_exists _ -> k (State.Fail error_syscall_exists)
      | Await prm ->
          Logs.debug (fun m ->
              m "[%a] await %a" Domain_uid.pp domain.uid Promise.pp prm);
          if Promise.is_a_children ~parent:current prm then k (await prm)
          else k (State.Fail Not_a_child)
      | Await_all prms ->
          if List.for_all (Promise.is_a_children ~parent:current) prms then
            k (await_all prms)
          else k (State.Fail Not_a_child)
      | Await_one (and_cancel, prms) ->
          if List.for_all (Promise.is_a_children ~parent:current) prms then
            k (await_one ~and_cancel pool domain prms)
          else k (State.Fail Not_a_child)
      | Both (prm0, prm1) ->
          if
            Promise.is_a_children ~parent:current prm0
            && Promise.is_a_children ~parent:current prm1
          then k (both prm0 prm1)
          else k (State.Fail Not_a_child)
      | Yield ->
          Logs.debug (fun m ->
              m "[%a] reschedule tasks" Domain_uid.pp domain.uid);
          (* TODO(dinosaure): we probably need to specify more strictly the
             behavior of [yield]. Indeed, depending on the [quanta], the result
             is not the same. It is also probably related to our note about
             [Arrived] (see [handle]) where we should start to consume effects
             via [State.run]. *)
          k State.Yield
      | Await_cancellation_of ([], and_return) ->
          Logs.debug (fun m ->
              m "[%a] nothing to await" Domain_uid.pp domain.uid);
          k (State.Send and_return)
      | Await_cancellation_of (prms, and_return) ->
          if List.for_all pack_is_cancelled prms then k (State.Send and_return)
          else
            let prms = List.filter (Fun.negate pack_is_cancelled) prms in
            k (await_cancellation_of prms ~and_return)
      | Cancel prm when Promise.is_cancelled prm ->
          Logs.debug (fun m ->
              m "[%a] %a cancels %a" Domain_uid.pp domain.uid Promise.pp current
                Promise.pp prm);
          if Promise.is_a_children ~parent:current prm then k (State.Send ())
          else k (State.Fail Not_a_child)
      | Cancel prm when Promise.is_consumed prm ->
          Logs.debug (fun m ->
              m "[%a] %a cancels %a" Domain_uid.pp domain.uid Promise.pp current
                Promise.pp prm);
          if Promise.is_a_children ~parent:current prm then k (State.Send ())
          else k (State.Fail Not_a_child)
      | Cancel prm ->
          Logs.debug (fun m ->
              m "[%a] %a cancels %a" Domain_uid.pp domain.uid Promise.pp current
                Promise.pp prm);
          if Promise.is_a_children ~parent:current prm then (
            terminate pool domain (Pack prm);
            k (await_cancellation_of [ Pack prm ] ~and_return:()))
          else k (State.Fail Not_a_child)
      | Suspend _ -> k State.Intr
      | Ownership action -> ownership current k action
      | effect -> k (State.None effect)
    in
    { State.perform }

  (* NOTE(dinosaure): [invariant] (used by [handle]) makes it possible to check
     the invariants in the case of [Await]. Indeed, the [Await] can cause the
     domain to wait (see [Condition.wait] & [run]) but we must make sure that
     these [Await] are right (and not wait for an impossible situation). Before
     adding the suspension of our tasks, we check if it respects our rules.

     The illegal case corresponds when a promise is passed from one domain to
     another and the latter "waits" for the said promise. The relationship
     (parent <-> children) does not exist, we should fail with [Not_a_child].
     However, it can happen that the domain reaches the [Condition.wait] point
     **before** having done the illegal "await" - in this case, and if it has
     nothing to do, the domain will do the [Condition. wait] thinking there is a
     pending task in another domain (although it may have already completed).

     XXX(dinosaure): This case is still relevant (and, currently, it is rather
     healthy to do this post-cleaning) but the [Condition.wait] of a domain
     poses other concerns (starvation). We preferred the simple idea that a
     domain could continue to work (and sometimes even go into a burning loop)
     than to deal with the subtleties of a [Condition.wait] that would block the
     whole program. *)
  let invariant current state =
    match state with
    | State.Suspended (k, Await prm) ->
        if Promise.is_a_children ~parent:current prm = false then
          State.discontinue_with k Not_a_child
        else state
    | State.Suspended (k, Await_all prms) ->
        if List.for_all (Promise.is_a_children ~parent:current) prms = false
        then State.discontinue_with k Not_a_child
        else state
    | State.Suspended (k, Await_one (_, prms)) ->
        if List.for_all (Promise.is_a_children ~parent:current) prms = false
        then State.discontinue_with k Not_a_child
        else state
    | state -> state

  let resource_leak prm =
    let leak = ref false in
    let f (Resource { uid; finaliser; value }) =
      Logs.debug (fun m ->
          m "resource [%a] leaked on %a" Resource_uid.pp uid Promise.pp prm);
      finaliser value;
      leak := true
    in
    Sequence.iter ~f prm.resources;
    !leak

  let handle pool domain prm = function
    | State.Finished (Error exn) when Promise.children_terminated prm ->
        Logs.debug (fun m ->
            m "%a failed with %S" Promise.pp prm (Printexc.to_string exn));
        let f (Resource { finaliser; value; _ }) = finaliser value in
        Sequence.iter ~f prm.resources;
        Promise.transition prm (Error exn)
    | State.Finished (Error exn) ->
        (* XXX(dinosaure): A subtlety operates here. Even if the function has
           just ended on an abnormal case, we keep the task as long as all its
           children are alive. We repeat the operation of canceling these
           children until they are all finished and it is only then that we
           apply the transition from Pending to the error case.

           It should be noted that we can sanely "pollute" our heap with several
           cancellations (even if they already exist in our heap). Indeed, it is
           cheaper to add an cancellation than to check if the cancellation has
           already been added. In the event that the cancellation appears but
           the promise has already been canceled, we do absolutely nothing (see
           the [once] function which consumes tasks). *)
        Queue.iter ~f:(terminate pool domain) prm.children;
        add_task domain (Suspended (prm, State.pure (Error exn)))
    | State.Finished (Ok value) ->
        Logs.debug (fun m -> m "%a resolved" Promise.pp prm);
        if Promise.children_terminated prm = false then raise Still_has_children
        else if resource_leak prm then begin
          Logs.err (fun m -> m "%a leaked a resource" Promise.pp prm);
          raise Resource_leak
        end
        else Promise.transition prm (Ok value)
    | State.Suspended (k, Suspend syscall) ->
        suspend_system_call domain syscall prm k
    | State.Suspended _ as state ->
        let state = invariant prm state in
        add_task domain (Suspended (prm, state))
    | State.Unhandled _ as state ->
        Logs.debug (fun m ->
            m "%a suspended due to unhandled effect" Promise.pp prm);
        let state = invariant prm state in
        add_task domain (Suspended (prm, state))

  let transfer_system_task pool domain (Continue (uid, fn0)) =
    match Hashtbl.find domain.system_tasks uid with
    | System_call_suspended (_, fn1, prm, k) ->
        Hashtbl.remove domain.system_tasks uid;
        Logs.debug (fun m ->
            m "[%a] consumes the system task [%a]" Domain_uid.pp domain.uid
              Syscall_uid.pp uid);
        let state =
          try
            fn0 ();
            State.continue_with k (fn1 ())
          with exn -> State.discontinue_with k exn
        in
        handle pool domain prm state
    | exception Not_found -> ()

  let clean_system_tasks domain prm =
    Hashtbl.filter_map_inplace
      (fun _ (System_call_suspended (_, _, prm', _) as suspended) ->
        if Promise.equal prm prm' then None else Some suspended)
      domain.system_tasks

  let once pool domain task =
    if not (task_is_cancelled task) then
      match task with
      | Tick -> Domain.cpu_relax ()
      | Arrived (prm, fn) ->
          Logs.debug (fun m ->
              m "[%a] %a arrived" Domain_uid.pp domain.uid Promise.pp prm);
          (* Note to myself. It seems that if we try to execute several "quanta"
             as soon as a task arrives, we have a performance regression with
             our example on the merkle-trees. It's quite difficult to know why,
             but let's keep this in mind if one day we want to be more truthful
             about our scheduler implementation.

             Indeed, stricto-sensus, we should still consume our "quantas" even
             if the task has just arrived. I think in Merkle's case that
             [hash_of_blob] has no "quanta"/effect. Running this function
             "multiple" times in a loop is more expensive than running it once
             and already getting the [Finished] state. *)
          handle pool domain prm (State.make fn ())
      | Suspended (prm, state) ->
          let perform = perform pool domain (Promise.pack prm) in
          let state = State.run ~quanta:domain.quanta ~perform state in
          handle pool domain prm state
      | Cancelled prm -> (
          Logs.debug (fun m ->
              m "[%a] %a cancelled" Domain_uid.pp domain.uid Promise.pp prm);
          clean_system_tasks domain prm;
          match clean_promise ~uid:prm.uid domain with
          | [ Suspended (_, state) ] ->
              Queue.iter ~f:(terminate pool domain) prm.children;
              let _state = State.fail ~exn:Cancelled state in
              Promise.transition prm (Error Cancelled);
              Promise.consume prm
          | [ Arrived (prm, _) ] ->
              Promise.transition prm (Error Cancelled);
              Promise.consume prm
          | [] -> Promise.cancel prm
          | _ -> assert false)

  let unblock_awaits_with_system_tasks pool domain =
    let open Effect.Deep in
    let retc = Fun.id in
    let exnc = reraise in
    let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
      function
      | Syscall_exists uid ->
          Some (fun k -> continue k (Hashtbl.mem domain.system_tasks uid))
      | _ -> None
    in
    let handler = { retc; exnc; effc } in
    let fn () =
      List.iter (transfer_system_task pool domain) (domain.events.select ())
    in
    match_with fn () handler

  let system_tasks_suspended domain = Hashtbl.length domain.system_tasks > 0

  let one_task_for ~domain (pool : pool) =
    let exception Yes in
    let f = function
      | Task (prm, _) when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace Yes
      | Cancel prm when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace Yes
      | _ -> ()
    in
    try
      Sequence.iter ~f pool.tasks;
      false
    with Yes -> true

  let run pool domain () =
    match Heapq.pop_minimum domain.tasks with
    | exception Heapq.Empty ->
        if system_tasks_suspended domain then
          unblock_awaits_with_system_tasks pool domain
    | _tick, elt ->
        once pool domain elt;
        if system_tasks_suspended domain then
          unblock_awaits_with_system_tasks pool domain
end

module Pool = struct
  let nothing_to_do (pool : pool) (domain : domain) =
    Heapq.is_empty domain.tasks
    && Domain.system_tasks_suspended domain = false
    && Domain.one_task_for ~domain pool = false

  let rec transfer_all_tasks (pool : pool) (domain : domain) =
    let exception Task of elt Sequence.node in
    let f node =
      match Sequence.data node with
      | Cancel prm when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace (Task node)
      | Task (prm, _) when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace (Task node)
      | _ -> ()
    in
    try Sequence.iter_node ~f pool.tasks
    with Task node ->
      let data = Sequence.data node in
      Sequence.remove node;
      begin
        match data with
        | Cancel prm -> Domain.add_task domain (Cancelled prm)
        | Task (prm, fn) -> Domain.add_task domain (Arrived (prm, fn))
      end;
      transfer_all_tasks pool domain

  (* The "worker" has a very simple operation: it waits for a signal from the
     other domains and sees if it has something to do. If so, it transfers
     ([transfer_all_tasks]) the tasks locally and runs [Domain.run]. Finally, it
     goes back to sleep until the next signal. Domains can communicate with
     [dom0] (the launcher) by signaling that all domains are dormant ([idle]).
     Finally, [dom0] can communicate with the domains asking to stop. *)
  let worker pool domain _ =
    let exception Exit in
    try
      while true do
        Mutex.lock pool.mutex;
        while nothing_to_do pool domain && not pool.stop do
          Condition.wait pool.condition_pending_work pool.mutex
        done;
        if pool.stop then raise_notrace Exit;
        transfer_all_tasks pool domain;
        pool.working_counter <- pool.working_counter + 1;
        Mutex.unlock pool.mutex;
        Domain.run pool domain ();
        Mutex.lock pool.mutex;
        pool.working_counter <- pool.working_counter - 1;
        if (not pool.stop) && Int.equal pool.working_counter 0 then
          Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex
      done
    with
    | Exit ->
        pool.domains_counter <- pool.domains_counter - 1;
        Condition.signal pool.condition_all_idle;
        Logs.debug (fun m -> m "[%a] terminates" Domain_uid.pp domain.uid);
        Mutex.unlock pool.mutex
    | exn ->
        (* NOTE(dinosaure): this exception appears only by [Domain.run pool domain ()].
           At this stage, [pool.mutex] is unlocked! We must re-lock it. *)
        Mutex.lock pool.mutex;
        pool.stop <- true;
        pool.fail <- true;
        pool.domains_counter <- pool.domains_counter - 1;
        Condition.broadcast pool.condition_pending_work;
        Condition.signal pool.condition_all_idle;
        Logs.err (fun m ->
            m "[%a] got an unexpected error: %S" Domain_uid.pp domain.uid
              (Printexc.to_string exn));
        Logs.err (fun m -> m "stop the pool of domains");
        Mutex.unlock pool.mutex;
        reraise exn

  (* This function is useful for [dom0], the domain that launched the others.
     The latter can be waiting for a result from the other domains to "move
     forward" (it only has "awaits" and it does not wait for system events). In
     this particular situation, instead of getting into a burning loop (and just
     waiting for a result), we can use [Condition.wait].

     Be careful, such a synchronization point can only exist because it is
     impossible for other domains to give a task to [dom0] - otherwise, we run
     into a starvation problem where the domains are all waiting for each other.
  *)
  let wait pool =
    let exception Exit in
    Mutex.lock pool.mutex;
    try
      let working_domains () = pool.working_counter > 0 && not pool.stop in
      let all_domains () = pool.stop && pool.domains_counter > 0 in
      while true do
        if working_domains () || all_domains () then
          Condition.wait pool.condition_all_idle pool.mutex
        else raise_notrace Exit
      done
    with Exit -> Mutex.unlock pool.mutex

  let kill pool =
    Mutex.lock pool.mutex;
    pool.stop <- true;
    Condition.broadcast pool.condition_pending_work;
    Mutex.unlock pool.mutex;
    wait pool

  let make ?(quanta = 2) ?(g = Random.State.make_self_init ())
      ?(handler = dummy_handler)
      ?(domains = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) events
      =
    let domains = List.init domains @@ fun _ -> Domain.make ~quanta ~g events in
    let pool =
      {
        tasks= Sequence.create (Random.State.copy g)
      ; mutex= Mutex.create ()
      ; condition_pending_work= Condition.create ()
      ; condition_all_idle= Condition.create ()
      ; stop= false
      ; fail= false
      ; working_counter= 0
      ; domains_counter= List.length domains
      ; domains
      }
    in
    (* NOTE(dinosaure): we apply the user's handler here but we probably use it
       when we call [Domain.run] as [dom0] does. *)
    let spawn domain =
      Stdlib.Domain.spawn (handler.handler (worker pool domain))
    in
    (pool, List.map spawn domains)
end

(** Functions and modules which are exposed to the interface. *)

module Ownership = struct
  type t = resource

  let own ~finally:finaliser value =
    let res = Resource.make ~finaliser value in
    Effect.perform (Ownership (Own res))

  let disown res = Effect.perform (Ownership (Disown res))
  let transfer res = Effect.perform (Ownership (Transfer res))
  let check res = Effect.perform (Ownership (Check res))
end

let domains () = Effect.perform Domains
let random () = Effect.perform Random
let self () = Effect.perform Self

type 'a orphans = 'a t Sequence.t

let orphans () =
  let g = Random.State.copy (random ()) in
  Sequence.create g

let care t =
  let exception Found in
  let ready = ref None in
  let f node =
    match Atomic.get (Sequence.data node).state with
    | Pending -> ()
    | _ ->
        ready := Some node;
        raise_notrace Found
  in
  if Sequence.length t > 0 then begin
    try Sequence.iter_node ~f t; Some None
    with Found ->
      let[@warning "-8"] (Some node) = !ready in
      let data = Sequence.data node in
      Sequence.remove node; Some (Some data)
  end
  else None

let call ?orphans ?(give = []) fn =
  let domains = domains () in
  if domains = [] then raise No_domain_available;
  let cur = Domain.self () in
  let uid =
    let g = random () in
    let l = List.filter (Fun.negate (Domain_uid.equal cur)) domains in
    List.nth l (Random.State.int g (List.length l))
  in
  let prm = Effect.perform (Spawn (Parallel uid, give, fn)) in
  Option.iter (Sequence.push prm) orphans;
  prm

let call_cc ?orphans ?(give = []) fn =
  let prm = Effect.perform (Spawn (Concurrent, give, fn)) in
  Option.iter (Sequence.push prm) orphans;
  prm

let make fn = Effect.perform (Syscall fn)
let suspend syscall = Effect.perform (Suspend syscall)
let await prm = Effect.perform (Await prm)
let yield () = Effect.perform Yield
let cancel prm = Effect.perform (Cancel prm)
let await_all prms = Effect.perform (Await_all prms)
let uid (Syscall (uid, _) : _ syscall) = uid

let parallel fn tasks =
  let domains = domains () in
  let current = Domain.self () in
  if domains = [] then raise No_domain_available;
  let domains = List.filter (Fun.negate (Domain_uid.equal current)) domains in
  let rec go rr prms tasks =
    match (tasks, rr) with
    | [], _ -> await_all (List.rev prms)
    | value :: rest, [ uid ] ->
        let prm =
          Effect.perform (Spawn (Parallel uid, [], fun () -> fn value))
        in
        go domains (prm :: prms) rest
    | value :: rest, uid :: domains ->
        let prm =
          Effect.perform (Spawn (Parallel uid, [], fun () -> fn value))
        in
        go domains (prm :: prms) rest
    | _, [] -> assert false
  in
  go domains [] tasks

let is_pending (Syscall (uid, _) : _ syscall) =
  Effect.perform (Syscall_exists uid)

let await_one prms =
  if prms = [] then invalid_arg "Miou.await_one";
  Effect.perform (Await_one (false, prms))

let await_first prms =
  if prms = [] then invalid_arg "Miou.await_first";
  Effect.perform (Await_one (true, prms))

external reraise : exn -> 'a = "%reraise"

let await_exn prm =
  match await prm with Ok value -> value | Error exn -> reraise exn

let both prm0 prm1 = Effect.perform (Both (prm0, prm1))
let task (Syscall (uid, _) : _ syscall) fn = Continue (uid, fn)

let quanta =
  match Sys.getenv_opt "MIOU_QUANTA" with
  | Some str -> ( try int_of_string str with _ -> 1)
  | None -> 1

(* NOTE(dinosaure): this function is a bit precise. We can probably relax it
   when domains signal [dom0] for any sleeps/wakeups. *)
let await_only_domains dom0 =
  let exception No in
  let f (_, task) =
    match task with
    | Suspended (_, State.Suspended (_, Both (prm0, prm1))) ->
        if prm0.runner = 0 || prm1.runner = 0 then raise_notrace No
    | Suspended (_, State.Suspended (_, Await prm)) ->
        if prm.runner = 0 then raise_notrace No
    | Suspended (_, State.Suspended (_, Await_all prms)) ->
        if List.exists (fun prm -> prm.runner = 0) prms then raise_notrace No
    | Suspended (_, State.Suspended (_, Await_one (_, prms))) ->
        if List.exists (fun prm -> prm.runner = 0) prms then raise_notrace No
    | _ -> raise_notrace No
  in
  if not (Heapq.is_empty dom0.tasks) then
    try Heapq.iter f dom0.tasks; true with No -> false
  else true

let run ?(quanta = quanta) ?(events = Fun.const dummy_events)
    ?(g = Random.State.make_self_init ()) ?domains ?(handler = dummy_handler) fn
    =
  Domain.Uid.reset ();
  let dom0 = Domain.make ~quanta ~g events in
  let prm0 = Promise.make ~resources:[] ~runner:dom0.uid () in
  Domain.add_task dom0 (Arrived (prm0, fn));
  let pool, domains = Pool.make ~quanta ~g ?domains ~handler events in
  let result =
    try
      while Promise.is_pending prm0 && not pool.fail do
        handler.handler (Domain.run pool dom0) ();
        if await_only_domains dom0 && Domain.system_tasks_suspended dom0 = false
        then Pool.wait pool
      done;
      if not pool.fail then Promise.to_result prm0
      else Error (Failure "A domain failed")
        (* XXX(dinosaure): if [pool.fail = true], a domain re-raised the exception it
           got during the process. Event if we return [Failure "A domain failed"], we
           should get the initial exception via [Domain.join]. *)
    with exn -> Error exn
  in
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  match result with Ok value -> value | Error exn -> raise exn
