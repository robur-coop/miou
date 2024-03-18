[@@@warning "-37"]

type impossible = |

external reraise : exn -> 'a = "%reraise"

module Queue = Queue
module State = State
module Heapq = Heapq
module Logs = Logs
module Fmt = Fmt
module Gen = Gen
open Sync
module Trigger = Trigger
module Computation = Computation
module Promise_uid = Gen.Make ()
module Domain_uid = Gen.Make ()
module Resource_uid = Gen.Make ()
module Syscall_uid = Gen.Make ()

exception Cancelled
exception Still_has_children
exception Not_a_child
exception No_domain_available

type 'a r = { uid: Resource_uid.t; value: 'a; finaliser: 'a -> unit }
type resource = Resource : 'a r -> resource [@@unboxed]

type 'a t = {
    uid: Promise_uid.t
  ; runner: Domain_uid.t
  ; mutable forbid: bool
  ; state: 'a Computation.t
  ; parent: pack option
  ; children: pack Sequence.t
  ; resources: resource Sequence.t
  ; mutable cancelled: (exn * Printexc.raw_backtrace) option
  ; cleaned: bool Atomic.t
}

and pack = Pack : 'a t -> pack [@@unboxed]

module Promise = struct
  let create ?parent ~forbid runner =
    {
      uid= Promise_uid.gen ()
    ; runner
    ; forbid
    ; state= Computation.create ()
    ; parent= Option.map (fun prm -> Pack prm) parent
    ; children= Sequence.create ()
    ; resources= Sequence.create ()
    ; cancelled= None
    ; cleaned= Atomic.make false
    }

  let pp ppf ({ uid; runner; _ } as prm) =
    Fmt.pf ppf "[%a:%a](%d)" Domain_uid.pp runner Promise_uid.pp uid
      Obj.(reachable_words (repr prm))

  let pp_pack ppf (Pack prm) = pp ppf prm
  let has_forbidden { forbid; _ } = forbid
  let children_terminated prm = Sequence.is_empty prm.children

  open struct
    let explicitely ~forbid prm fn =
      if prm.forbid = forbid then fn ()
      else
        match fn (prm.forbid <- true) with
        | value ->
            prm.forbid <- not forbid;
            value
        | exception exn ->
            prm.forbid <- not forbid;
            raise exn
  end

  let forbid t fn = explicitely ~forbid:true t fn
end

exception Clean_children of pack Sequence.node

(* - the domain which runs this function **must** be the runner of [self].
   - [self] is **probably** the parent of [child] but, in some situation,
     [clean_children] is called when [self] is a /proxy promise/ like the one
     created and used in [await_{first,one}]. From our previous assertion, even
     if [self] is not the parent of [child], we can safely execute this code.
   - we prevent the case where [self] is not the parent of [child], and undoes
     an unnecessary iteration ([Sequence.iter_node])
*)
let clean_children ~self (child : _ t) =
  let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  assert (Domain_uid.equal self.runner runner);
  Logs.debug (fun m ->
      m "[%a] clean %a into %a" Domain_uid.pp runner Promise.pp child Promise.pp
        self);
  let f (Sequence.{ data= Pack prm; _ } as node) =
    if Promise_uid.equal child.uid prm.uid then
      raise_notrace (Clean_children node)
  in
  let some (Pack prm) = Promise_uid.equal self.uid prm.uid in
  if Option.fold ~none:false ~some child.parent then
    try Sequence.iter_node ~f self.children
    with Clean_children node -> Sequence.remove node

type syscall = Syscall : Syscall_uid.t * Trigger.t * _ t -> syscall
type signal = Signal : Trigger.t * _ t -> signal
type uid = Syscall_uid.t
type select = block:bool -> uid list -> signal list
type events = { select: select; interrupt: unit -> unit }

type domain_elt =
  | Domain_create : resource list * 'a t * (unit -> 'a) -> domain_elt
  | Domain_cancel : _ t * Printexc.raw_backtrace -> domain_elt
  | Domain_clean : _ t * _ t -> domain_elt
  | Domain_task : 'a t * 'a State.t -> domain_elt
  | Domain_tick : impossible -> domain_elt

let promise_of_domain_elt = function
  | Domain_tick _ -> .
  | Domain_create (_, prm, _) -> Pack prm
  | Domain_cancel (prm, _) -> Pack prm
  | Domain_clean (prm, _) -> Pack prm
  | Domain_task (prm, _) -> Pack prm

type domain = {
    uid: Domain_uid.t
  ; tasks: domain_elt Heapq.t
  ; quanta: int
  ; g: Random.State.t
  ; events: events
  ; cancelled_syscalls: Syscall_uid.t Queue.t
  ; syscalls: int Atomic.t
}

type 'r continuation = (State.error option, 'r) State.continuation

type pool_elt =
  | Pool_create : resource list * 'a t * (unit -> 'a) -> pool_elt
  | Pool_cancel : _ t * Printexc.raw_backtrace -> pool_elt
  | Pool_clean : _ t * _ t -> pool_elt
  | Pool_continue : {
        prm: 'r t
      ; result: State.error option
      ; k: 'r continuation
    }
      -> pool_elt

let promise_of_pool_elt = function
  | Pool_create (_, prm, _) -> Pack prm
  | Pool_cancel (prm, _) -> Pack prm
  | Pool_clean (prm, _) -> Pack prm
  | Pool_continue { prm; _ } -> Pack prm

type dom0_elt =
  | Dom0_continue : {
        prm: 'r t
      ; result: State.error option
      ; k: 'r continuation
    }
      -> dom0_elt
  | Dom0_clean : _ t * _ t -> dom0_elt

type pool = {
    tasks: pool_elt Sequence.t
  ; mutex: Mutex.t
  ; condition_pending_work: Condition.t
  ; condition_all_idle: Condition.t
  ; domains: domain list
  ; dom0: domain
  ; to_dom0: dom0_elt Queue.t
  ; mutable stop: bool
  ; mutable fail: bool
  ; mutable working_counter: int
  ; mutable domains_counter: int
}

let get_domain_from_uid pool ~uid =
  List.find (fun domain -> Domain_uid.equal domain.uid uid) pool.domains

[@@@warning "-38"]

type ty = Concurrent | Parallel of Domain_uid.t
type 'r waiter = { pool: pool; domain: domain; prm: 'r t }
type _ Effect.t += Spawn : ty * bool * (unit -> 'a) -> 'a t Effect.t
type _ Effect.t += Self : pack Effect.t
type _ Effect.t += Self_domain : domain Effect.t
type _ Effect.t += Get : 'a -> 'a Effect.t
type _ Effect.t += Random : Random.State.t Effect.t
type _ Effect.t += Domains : Domain_uid.t list Effect.t
type _ Effect.t += Await_cancellation : _ t -> unit Effect.t
type _ Effect.t += Cancel : Printexc.raw_backtrace * 'a t -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Stats : unit Effect.t

let pp_effect : type a. a Effect.t Fmt.t =
 fun ppf -> function
  | Spawn _ -> Fmt.string ppf "Spawn"
  | Self -> Fmt.string ppf "Self"
  | Self_domain -> Fmt.string ppf "Self_domain"
  | Get _ -> Fmt.string ppf "Get"
  | Random -> Fmt.string ppf "Random"
  | Domains -> Fmt.string ppf "Domains"
  | Await_cancellation prm ->
      Fmt.pf ppf "@[<1>(Await_cancellation@ %a)@]" Promise.pp prm
  | Cancel (_, prm) -> Fmt.pf ppf "@[<1>(Cancel@ %a)@]" Promise.pp prm
  | Yield -> Fmt.string ppf "Yield"
  | Stats -> Fmt.string ppf "Stats"
  | Trigger.Await _ -> Fmt.string ppf "Await"
  | _ -> Fmt.string ppf "#effect"

let pp_domain_elt ppf = function
  | Domain_create (_ress, prm, _fn) ->
      Fmt.pf ppf "@[<1>(Domain_create@ %a)@]" Promise.pp prm
  | Domain_cancel (prm, _) ->
      Fmt.pf ppf "@[<1>(Domain_cancel@ %a)@]" Promise.pp prm
  | Domain_clean (prm, child) ->
      Fmt.pf ppf "@[<1>(Domain_clean@ @[<1>(%a,@ %a0@])@]" Promise.pp prm
        Promise.pp child
  | Domain_task (prm, State.Finished _) ->
      Fmt.pf ppf "@[<1>(Domain_task@ %a:finished)@]" Promise.pp prm
  | Domain_task (prm, State.Suspended (_, eff)) ->
      Fmt.pf ppf "@[<1>(Suspended@ @[<1>(%a,@ %a)@])@]" Promise.pp prm pp_effect
        eff
  | Domain_task (prm, State.Unhandled _) ->
      Fmt.pf ppf "@[<1>(Domain_task@ %a:unhandled)@]" Promise.pp prm
  | Domain_tick _ -> .

module Domain = struct
  module Uid = Domain_uid

  let domain_elt_to_int = function
    | Domain_clean _ -> 1
    | Domain_cancel _ -> 2
    | _ -> 3

  let domain_elt_compare a b = domain_elt_to_int a - domain_elt_to_int b
  let domain_elt_dummy = Domain_tick (Obj.magic ())

  let create ?(quanta = 1) ~events g uid =
    let tasks =
      Heapq.create ~compare:domain_elt_compare ~dummy:domain_elt_dummy 0x100
    in
    {
      uid
    ; tasks
    ; quanta
    ; g
    ; events= events uid
    ; syscalls= Atomic.make 0
    ; cancelled_syscalls= Queue.create ()
    }

  let interrupt pool ~domain:uid =
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    Logs.debug (fun m ->
        m "[%a] interrupts [%a]" Domain_uid.pp runner Domain_uid.pp uid);
    let domain =
      List.find
        (fun dom -> Domain_uid.equal uid dom.uid)
        (pool.dom0 :: pool.domains)
    in
    domain.events.interrupt ()

  (* These functions can be used to add items to be done to the various domains.
     There are 3 cases:
     - Add an item to the domain used to execute the code:
       [assert (Stdlib.Domain.self () = domain.uid)]
     - Adding an item to a domain via the pool. This operation has no
       constraints but uses a mutex for synchronisation.
     - Adding an item to [dom0]. [dom0] has its own synchronisation mechanism
       with an atomic queue, so there are no constraints. [dom0] also accepts a
       more restricted set of operations than the other domains (for example,
       you cannot create a task in [dom0]).

     [add_into_domain] can be used if you are in [dom0] and want to add an item
     to [dom0]. In other words, [add_into_domain] is intra-domain communication,
     whereas [add_into_pool] and [add_into_dom0] are inter-domain communication.
  *)

  let add_into_domain domain elt =
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    assert (Domain_uid.equal runner domain.uid);
    Heapq.add domain.tasks elt

  let add_into_pool pool elt =
    Mutex.lock pool.mutex;
    let direction =
      match elt with Pool_cancel _ -> Sequence.Left | _ -> Sequence.Right
    in
    Sequence.add direction pool.tasks elt;
    Condition.broadcast pool.condition_pending_work;
    Mutex.unlock pool.mutex;
    let (Pack prm) = promise_of_pool_elt elt in
    interrupt pool ~domain:prm.runner

  let add_into_dom0 pool elt = Queue.enqueue pool.to_dom0 elt

  let cancel pool domain ~backtrace:bt prm =
    (* The promise given does not necessarily belong to the [domain] currently
       in use (the assertion [prm.runner <> domain.uid] exists). However, it is
       impossible for a promise belonging to [dom0] to be executed by another
       domain ([domain.uid <> 0]). We put an [assert false] about such case. *)
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    assert (Domain_uid.equal domain.uid runner);
    Logs.debug (fun m ->
        m "[%a] signals to cancel %a" Domain_uid.pp domain.uid Promise.pp prm);
    match (Domain_uid.to_int prm.runner, Domain_uid.to_int runner) with
    | 0, 0 -> add_into_domain domain (Domain_cancel (prm, bt))
    | 0, _ -> assert false
    | _ -> add_into_pool pool (Pool_cancel (prm, bt))

  let handle pool domain prm state =
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    assert (Domain_uid.equal runner domain.uid);
    assert (Domain_uid.equal prm.runner domain.uid);
    match state with
    | (State.Suspended _ | State.Unhandled _) as state ->
        add_into_domain domain (Domain_task (prm, state))
    | State.Finished (Error (exn, bt)) ->
        let f (Pack prm) = cancel pool domain ~backtrace:bt prm in
        Sequence.iter ~f prm.children;
        ignore (Computation.try_cancel prm.state (exn, bt));
        assert (Option.is_some (Computation.cancelled prm.state))
    | State.Finished (Ok value) ->
        if Promise.children_terminated prm = false then
          raise_notrace Still_has_children;
        Logs.debug (fun m ->
            m "[%a] %a finished correctly" Domain_uid.pp domain.uid Promise.pp
              prm);
        ignore (Computation.try_return prm.state value)

  let propagate_cancellation trigger (pool, possibly_cancelled) child =
    match Computation.cancelled possibly_cancelled.state with
    | None -> Computation.detach possibly_cancelled.state trigger
    | Some (exn, bt) -> (
        let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
        let cancelled = possibly_cancelled in
        ignore (Computation.try_cancel child.state (exn, bt));
        match (Domain_uid.to_int child.runner, Domain_uid.to_int runner) with
        | 0, 0 -> add_into_domain pool.dom0 (Domain_cancel (child, bt))
        | 0, _ -> assert false
        | _ ->
            (* [propagate_cancellation] can be called by the [child] or its
               parent ([possibly_cancelled]). Promises can exist in 2 different
               domains. We therefore need to check that the domain that executes
               the function is the same as the one that holds the parent. *)
            if
              Domain_uid.equal child.runner runner
              && Domain_uid.equal cancelled.runner runner
            then
              let domain = get_domain_from_uid pool ~uid:cancelled.runner in
              add_into_domain domain (Domain_cancel (child, bt))
            else add_into_pool pool (Pool_cancel (child, bt)))

  let canceller pool ~self (child : _ t) =
    let trigger = Trigger.create () in
    assert (Trigger.on_signal trigger (pool, self) child propagate_cancellation);
    assert (Computation.try_attach self.state trigger);
    assert (Computation.try_attach child.state trigger)

  let perform : pool -> domain -> 'x t -> State.perform =
   fun pool domain prm ->
    let open State in
    let perform : type a b. (a, b) State.handler =
     fun k eff ->
      match eff with
      | Get value -> k (Operation.return value)
      | Self -> k (Operation.return (Pack prm))
      | Self_domain -> k (Operation.return domain)
      | Random -> k (Operation.return domain.g)
      | Domains ->
          let domains = List.map (fun { uid; _ } -> uid) pool.domains in
          k (Operation.return domains)
      | Spawn (Concurrent, forbid, fn) ->
          let prm' = Promise.create ~parent:prm ~forbid domain.uid in
          canceller pool ~self:prm prm';
          Sequence.(add Left) prm.children (Pack prm');
          add_into_domain domain (Domain_create ([], prm', fn));
          k (Operation.return prm')
      | Spawn (Parallel runner, forbid, fn) ->
          let prm' = Promise.create ~parent:prm ~forbid runner in
          canceller pool ~self:prm prm';
          Sequence.(add Left) prm.children (Pack prm');
          add_into_pool pool (Pool_create ([], prm', fn));
          k (Operation.return prm')
      | Cancel (backtrace, child) ->
          cancel pool domain ~backtrace child;
          k (Operation.continue (Await_cancellation child))
      | Await_cancellation child as await ->
          if
            Computation.is_running child.state = false
            && Sequence.is_empty child.children
            && Atomic.get child.cleaned
          then k (Operation.return (clean_children ~self:prm child))
          else k (Operation.continue await)
      | Trigger.Await _ -> k Operation.interrupt
      | Yield -> k Operation.yield
      | Stats -> k (Operation.return ())
      | effect -> k (Operation.perform effect)
    in
    { perform }

  let resume : type r. Trigger.t -> r waiter -> r continuation -> unit =
   fun trigger { pool; domain; prm } k ->
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    if not (Promise.has_forbidden prm) then Computation.detach prm.state trigger;
    let result = Computation.cancelled prm.state in
    match Domain_uid.to_int prm.runner with
    | 0 -> add_into_dom0 pool (Dom0_continue { prm; result; k })
    | _ when Domain_uid.equal runner domain.uid ->
        let state = State.suspended_with k (Get result) in
        add_into_domain domain (Domain_task (prm, state))
    | _ -> add_into_pool pool (Pool_continue { prm; result; k })

  (* It should be noted that continuation of [k] does not mean that the promise
     has ended (it means that the promise **may** have ended). We can find out
     whether the promise has indeed been cancelled (if we give a [Some] value),
     but we need to **re-observe** its status to find out whether it has been
     resolved!

     In other words, await only mentions a change of state towards
     cancellation. *)
  let await pool domain prm trigger k =
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    assert (Domain_uid.equal runner domain.uid);
    assert (Domain_uid.equal prm.runner domain.uid);
    let waiter = { pool; domain; prm } in
    match Promise.has_forbidden prm with
    | true ->
        (* The promise has forbidden propagation of cancellation. We try to
           suspend ([Trigger.on_signal]) [k] and do a next iteration. If we can
           not suspend, the trigger was already signaled, so we must continue
           [k] with [None] (we don't propagate the possible cancellation). *)
        if not (Trigger.on_signal trigger waiter k resume) then
          let state = State.suspended_with k (Get None) in
          add_into_domain domain (Domain_task (prm, state))
    | false ->
        (* In that case, we must propagate the cancellation if [prm.state] has
           ended abnormally. We try to attach tghe trigger to the compuatation
           [prm.state]. Two situations are possible:
           - if we can attach the trigger, this means that the promise has not
             yet been cancelled/resolved. However, between attachment and
             suspension ([Trigger.on_signal]), this can be the case. Once
             suspended, we can be sure that [resume] will be called as soon as
             the state of our computation changes. Otherwise, we must continue
             [k] (and clean up our last attachment) with the current state of
             our promise.
           - if we can't attach the trigger, it means that either the promise
             has been resolved/cancelled or the trigger has been signalled. In
             these cases, we make sure that the trigger really has been
             signalled and we continue [k] with the state of the promise. *)
        if Computation.try_attach prm.state trigger then (
          if not (Trigger.on_signal trigger waiter k resume) then (
            Computation.detach prm.state trigger;
            let result = Computation.cancelled prm.state in
            let state = State.suspended_with k (Get result) in
            add_into_domain domain (Domain_task (prm, state))))
        else
          let () = Trigger.signal trigger in
          let result = Computation.cancelled prm.state in
          let state = State.suspended_with k (Get result) in
          add_into_domain domain (Domain_task (prm, state))

  let get_elt_into_domain ~uid (domain : domain) =
    let elts = ref [] in
    let f = function
      | Domain_tick _ -> .
      | Domain_cancel _ | Domain_clean _ -> ()
      | elt ->
          let (Pack prm) = promise_of_domain_elt elt in
          if Promise_uid.equal prm.uid uid then elts := elt :: !elts
    in
    Heapq.iter f domain.tasks;
    match !elts with
    | [] -> None
    | [ (Domain_create _ as elt) ] | [ (Domain_task _ as elt) ] -> Some elt
    | _elts -> assert false

  let signal_to_clean_children pool domain prm =
    match prm.parent with
    | None -> ()
    | Some (Pack parent) -> (
        let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
        assert (Domain_uid.equal prm.runner runner);
        assert (Domain_uid.equal domain.uid runner);
        match (Domain_uid.to_int parent.runner, Domain_uid.to_int runner) with
        | 0, 0 -> add_into_domain domain (Domain_clean (parent, prm))
        | 0, _ -> add_into_dom0 pool (Dom0_clean (parent, prm))
        | _, 0 -> assert false
        | _ -> add_into_pool pool (Pool_clean (parent, prm)))

  let once pool domain = function
    | Domain_tick _ -> .
    | Domain_create (resources, prm, fn) -> (
        match Computation.cancelled prm.state with
        | None ->
            List.iter Sequence.(add Left prm.resources) resources;
            let state = State.make fn () in
            handle pool domain prm state
        | Some (exn, bt) ->
            Logs.debug (fun m ->
                m "[%a] %a was cancelled" Domain_uid.pp domain.uid Promise.pp
                  prm);
            let state = State.pure (Error (exn, bt)) in
            Atomic.set prm.cleaned true;
            handle pool domain prm state)
    | Domain_task (prm, State.Suspended (k, Trigger.Await trigger)) ->
        Logs.debug (fun m ->
            m "[%a] %a await" Domain_uid.pp domain.uid Promise.pp prm);
        await pool domain prm trigger k
    | Domain_task (prm, state) -> (
        let perform = perform pool domain prm in
        match Computation.cancelled prm.state with
        | None ->
            (* It is normally safe to run our continuation here without someone
               else doing it. So we shouldn't get the
               [Continuation_already_resumed] exception. If we do, it's a
               violation of our rules, namely that only the domain in charge of
               the promise can continue the continuation [k]. *)
            let state = State.run ~quanta:domain.quanta ~perform state in
            handle pool domain prm state
        | Some (exn, bt) ->
            let state = State.fail ~backtrace:bt ~exn state in
            Atomic.set prm.cleaned true;
            handle pool domain prm state)
    | Domain_clean (prm, child) -> clean_children ~self:prm child
    | Domain_cancel (prm, bt) as cancellation ->
        Logs.debug (fun m ->
            m "[%a] cancels computation %a" Domain_uid.pp domain.uid Promise.pp
              prm);
        ignore (Computation.try_cancel prm.state (Cancelled, bt));
        Logs.debug (fun m ->
            m "[%a] clean-up continuation of %a" Domain_uid.pp domain.uid
              Promise.pp prm);
        let () =
          match get_elt_into_domain domain ~uid:prm.uid with
          | Some (Domain_tick _) -> .
          | Some (Domain_create (_, prm', _)) ->
              assert (Promise_uid.equal prm.uid prm'.uid);
              assert (Domain_uid.equal prm.runner domain.uid);
              let state = State.pure (Error (Cancelled, bt)) in
              handle pool domain prm state
          | Some (Domain_task (prm', state)) ->
              assert (Promise_uid.equal prm.uid prm'.uid);
              assert (Domain_uid.equal prm.runner domain.uid);
              let state = State.fail ~backtrace:bt ~exn:Cancelled state in
              handle pool domain prm' state
          | Some (Domain_cancel _) | Some (Domain_clean _) | None -> ()
        in
        Atomic.set prm.cleaned true;
        Logs.debug (fun m ->
            m "[%a] clean-up children (%a) of %a" Domain_uid.pp domain.uid
              Promise.pp prm
              Fmt.(Dump.option Promise.pp_pack)
              prm.parent);
        if Sequence.is_empty prm.children then
          signal_to_clean_children pool domain prm
        else add_into_domain domain cancellation

  let signal_system_events domain (Signal (trigger, prm)) =
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    assert (Domain_uid.equal runner domain.uid);
    assert (Domain_uid.equal runner prm.runner);
    Logs.debug (fun m ->
        m "[%a] signals syscall into %a" Domain_uid.pp runner Promise.pp prm);
    try Trigger.signal trigger
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      (* TODO(dinosaure): we actually loose [exn] (and replace it then by
         [Cancelled]). *)
      ignore (Computation.try_cancel prm.state (exn, bt));
      add_into_domain domain (Domain_cancel (prm, bt))

  let unblock_awaits_with_system_events (domain : domain) =
    let block = Heapq.length domain.tasks = 0 in
    let cancelled = Queue.(to_list (transfer domain.cancelled_syscalls)) in
    let syscalls = domain.events.select ~block cancelled in
    List.iter (signal_system_events domain) syscalls

  let system_events_suspended domain = Atomic.get domain.syscalls > 0

  let run pool (domain : domain) =
    match Heapq.pop_minimum domain.tasks with
    | exception Heapq.Empty ->
        if system_events_suspended domain then
          unblock_awaits_with_system_events domain
    | elt ->
        Logs.debug (fun m ->
            m "[%a] does %a (%dw)" Domain_uid.pp domain.uid pp_domain_elt elt
              Obj.(reachable_words (repr elt)));
        once pool domain elt;
        if system_events_suspended domain then
          unblock_awaits_with_system_events domain

  let self () =
    let { uid; _ } = Effect.perform Self_domain in
    uid
end

module Pool = struct
  let one_task_for ~domain pool =
    let exception Yes in
    let f elt =
      let (Pack prm) = promise_of_pool_elt elt in
      if Domain_uid.equal prm.runner domain.uid then raise_notrace Yes
    in
    try
      Sequence.iter ~f pool.tasks;
      false
    with Yes -> true

  let _pp_stats ppf ((pool : pool), (domain : domain)) =
    Fmt.pf ppf
      "[pool:%dw, tasks:%dw, to_dom0:%dw, domain:%dw, tasks:%dw:%de, \
       cancelled_syscalls:%dw, live-words:%dw]"
      Obj.(reachable_words (repr pool))
      Obj.(reachable_words (repr pool.tasks))
      Obj.(reachable_words (repr pool.to_dom0))
      Obj.(reachable_words (repr domain))
      Obj.(reachable_words (repr domain.tasks))
      (Heapq.length domain.tasks)
      Obj.(reachable_words (repr domain.cancelled_syscalls))
      Gc.((quick_stat ()).live_words)

  let nothing_to_do (pool : pool) (domain : domain) =
    Heapq.is_empty domain.tasks
    && one_task_for ~domain pool = false
    && Atomic.get domain.syscalls = 0

  let transfer_all_tasks (pool : pool) (domain : domain) =
    let nodes = ref [] in
    let f ({ Sequence.data; _ } as node) =
      let (Pack prm) = promise_of_pool_elt data in
      if Domain_uid.equal prm.runner domain.uid then nodes := node :: !nodes
    in
    Sequence.iter_node ~f pool.tasks;
    let f ({ Sequence.data; _ } as node) =
      Sequence.remove node;
      match data with
      | Pool_create (ress, prm, fn) -> Domain_create (ress, prm, fn)
      | Pool_cancel (prm, bt) -> Domain_cancel (prm, bt)
      | Pool_clean (prm, child) -> Domain_clean (prm, child)
      | Pool_continue { prm; result; k } ->
          let state = State.suspended_with k (Get result) in
          Domain_task (prm, state)
    in
    let elts = List.map f !nodes in
    let elts = List.sort Domain.domain_elt_compare elts in
    List.iter (Domain.add_into_domain domain) elts

  let worker pool domain =
    let exception Exit in
    try
      while true do
        Mutex.lock pool.mutex;
        Logs.debug (fun m ->
            m "[%a] nothing to do? %b" Domain_uid.pp domain.uid
              (nothing_to_do pool domain));
        while nothing_to_do pool domain && not pool.stop do
          Condition.wait pool.condition_pending_work pool.mutex
        done;
        if pool.stop then raise_notrace Exit;
        transfer_all_tasks pool domain;
        pool.working_counter <- pool.working_counter + 1;
        Mutex.unlock pool.mutex;
        Logs.debug (fun m -> m "[%a] wake-up" Domain_uid.pp domain.uid);
        Domain.run pool domain;
        Mutex.lock pool.mutex;
        pool.working_counter <- pool.working_counter - 1;
        if (not pool.stop) && Int.equal pool.working_counter 0 then
          Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex
      done
    with
    | Exit ->
        Logs.debug (fun m -> m "[%a] exits" Domain_uid.pp domain.uid);
        pool.domains_counter <- pool.domains_counter - 1;
        Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex
    | exn ->
        Mutex.lock pool.mutex;
        pool.stop <- true;
        pool.fail <- true;
        pool.domains_counter <- pool.domains_counter - 1;
        Condition.broadcast pool.condition_pending_work;
        Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex;
        reraise exn

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

  let number_of_domains () =
    max 0 (Stdlib.Domain.recommended_domain_count () - 1)

  let create ?(quanta = 1) ~dom0 ?(domains = number_of_domains ()) ~events () =
    let domains =
      List.init domains @@ fun _ ->
      Domain.create ~quanta ~events dom0.g (Domain_uid.gen ())
    in
    let pool =
      {
        tasks= Sequence.create ()
      ; mutex= Mutex.create ()
      ; condition_pending_work= Condition.create ()
      ; condition_all_idle= Condition.create ()
      ; stop= false
      ; fail= false
      ; working_counter= 0
      ; domains_counter= List.length domains
      ; domains
      ; dom0
      ; to_dom0= Queue.create ()
      }
    in
    let spawn domain =
      Stdlib.Domain.spawn @@ fun () ->
      let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
      assert (Domain_uid.equal runner domain.uid);
      Logs.debug (fun m -> m "spawn the domain [%a]" Domain_uid.pp runner);
      worker pool domain
    in
    (pool, List.map spawn domains)
end

let call_cc ?orphans fn =
  let prm = Effect.perform (Spawn (Concurrent, false, fn)) in
  Option.iter (fun s -> Sequence.(add Left) s prm) orphans;
  prm

let call ?orphans fn =
  let domains = Effect.perform Domains in
  if domains = [] then raise No_domain_available;
  let (Pack self) = Effect.perform Self in
  let g = Effect.perform Random in
  let runner =
    match List.filter (Fun.negate (Domain_uid.equal self.runner)) domains with
    | [] -> raise No_domain_available
    | lst -> List.nth lst (Random.State.int g (List.length lst))
  in
  let prm = Effect.perform (Spawn (Parallel runner, false, fn)) in
  Option.iter (fun s -> Sequence.(add Left) s prm) orphans;
  prm

let await prm =
  let (Pack self) = Effect.perform Self in
  let some (Pack parent) = Promise_uid.equal self.uid parent.uid in
  if not (Option.fold ~none:false ~some prm.parent) then
    raise_notrace Not_a_child;
  let finally () = clean_children ~self prm in
  Fun.protect ~finally @@ fun () ->
  match Computation.await prm.state with
  | Ok _ as value -> Option.fold ~none:value ~some:Result.error prm.cancelled
  | Error (exn, bt) ->
      let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
      Logs.debug (fun m ->
          m "[%a] %a was cancelled" Domain_uid.pp runner Promise.pp prm);
      if Option.is_none prm.cancelled then prm.cancelled <- Some (exn, bt);
      Error (Option.get prm.cancelled)

let await_exn prm =
  match await prm with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let await prm = await prm |> Result.map_error fst

let await_one prms =
  if prms = [] then invalid_arg "Miou.await_one";
  let (Pack self) = Effect.perform Self in
  let some (Pack parent) = Promise_uid.equal self.uid parent.uid in
  let is_a_child prm = Option.fold ~none:false ~some prm.parent in
  if not (List.for_all is_a_child prms) then raise_notrace Not_a_child;
  let c = Computation.create () in
  let choose =
    Computation.try_capture c @@ fun () ->
    let t = Trigger.create () in
    let rec find_first_and_detach_rest = function
      | [] -> assert false
      | prm :: prms ->
          if Computation.is_running prm.state then (
            Computation.detach prm.state t;
            find_first_and_detach_rest prms)
          else (
            List.iter (fun prm -> Computation.detach prm.state t) prms;
            let result = Computation.await prm.state in
            clean_children ~self prm; result)
    and try_attach_all attached = function
      | prm :: prms ->
          let attached = prm :: attached in
          if Computation.try_attach prm.state t then
            try_attach_all attached prms
          else find_first_and_detach_rest attached
      | [] -> (
          match Trigger.await t with
          | Some (exn, bt) ->
              List.iter (fun prm -> Computation.detach prm.state t) attached;
              Error (exn, bt)
          | None -> find_first_and_detach_rest prms)
    in
    try_attach_all [] prms
  in
  let prm = call_cc choose in
  assert (await_exn prm);
  match Computation.await_exn c with
  | Ok value -> Ok value
  | Error (exn, _bt) -> Error exn
  | exception _exn -> assert false

let cancel ~self ~backtrace:bt prm =
  let finally () = clean_children ~self prm in
  Fun.protect ~finally @@ fun () -> Effect.perform (Cancel (bt, prm))

let await_first prms =
  if prms = [] then invalid_arg "Miou.await_first";
  let (Pack self) = Effect.perform Self in
  let some (Pack parent) = Promise_uid.equal self.uid parent.uid in
  let is_a_child prm = Option.fold ~none:false ~some prm.parent in
  let bt = Printexc.get_callstack max_int in
  if not (List.for_all is_a_child prms) then raise_notrace Not_a_child;
  let c = Computation.create () in
  let choose =
    Computation.try_capture c @@ fun () ->
    let t = Trigger.create () in
    let rec find_first_and_cancel_rest = function
      | [] -> assert false
      | prm :: prms ->
          if Computation.is_running prm.state then (
            Computation.detach prm.state t;
            cancel ~self ~backtrace:bt prm;
            find_first_and_cancel_rest prms)
          else (
            List.iter (fun prm -> Computation.detach prm.state t) prms;
            List.iter (cancel ~self ~backtrace:bt) prms;
            let result = Computation.await prm.state in
            clean_children ~self prm; result)
    and try_attach_all attached = function
      | prm :: prms ->
          let attached = prm :: attached in
          if Computation.try_attach prm.state t then
            try_attach_all attached prms
          else
            let () = List.iter (cancel ~self ~backtrace:bt) prms in
            find_first_and_cancel_rest attached
      | [] -> (
          match Trigger.await t with
          | Some (exn, bt) ->
              List.iter (fun prm -> Computation.detach prm.state t) attached;
              Error (exn, bt)
          | None -> find_first_and_cancel_rest prms)
    in
    try_attach_all [] prms
  in
  let prm = call_cc choose in
  assert (await_exn prm);
  match Computation.await_exn c with
  | Ok value -> Ok value
  | Error (exn, _bt) -> Error exn
  | exception _exn -> assert false

let cancel prm =
  let (Pack self) = Effect.perform Self in
  let some (Pack parent) = Promise_uid.equal self.uid parent.uid in
  if not (Option.fold ~none:false ~some prm.parent) then
    raise_notrace Not_a_child;
  let bt = Printexc.get_callstack max_int in
  let finally () = clean_children ~self prm in
  Fun.protect ~finally @@ fun () ->
  Effect.perform (Cancel (bt, prm));
  prm.cancelled <- Some (Cancelled, bt)

let await_all prms =
  let prms = List.rev_map (fun prm -> await prm) prms in
  List.rev prms

let parallel fn tasks =
  let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  let domains = Effect.perform Domains in
  let domains = List.filter (Fun.negate (Domain_uid.equal runner)) domains in
  let spawn runner fn v =
    let fn () = fn v in
    Effect.perform (Spawn (Parallel runner, false, fn))
  in
  if domains = [] then raise No_domain_available;
  let rec go rr prms tasks =
    match (tasks, rr) with
    | [], _ -> List.rev (await_all prms)
    | v :: rest, [ runner ] ->
        let prm = spawn runner fn v in
        go domains (prm :: prms) rest
    | v :: rest, runner :: domains ->
        let prm = spawn runner fn v in
        go domains (prm :: prms) rest
    | _, [] -> assert false
  in
  go domains [] tasks

let yield () = Effect.perform Yield
let stats () = Effect.perform Stats

let syscall () =
  let uid = Syscall_uid.gen () in
  let trigger = Trigger.create () in
  let (Pack self) = Effect.perform Self in
  let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  let domain = Effect.perform Self_domain in
  assert (Domain_uid.equal runner domain.uid);
  assert (Domain_uid.equal runner self.runner);
  Syscall (uid, trigger, self)

let suspend (Syscall (uid, trigger, prm)) =
  let domain = Effect.perform Self_domain in
  let (Pack self) = Effect.perform Self in
  if Promise_uid.equal self.uid prm.uid = false then
    invalid_arg "This syscall does not belong to the current promise";
  let runner' = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  assert (Domain_uid.equal domain.uid runner');
  if Domain_uid.equal prm.runner runner' = false then
    invalid_arg "This syscall does not belong to the current domain";
  Atomic.incr domain.syscalls;
  (* [Trigger.await] gives an opportunity to the scheduler to cancel the current
     continuation. Indeed, we produce a [(State.error, 'a) continuation] and the
     parent can actually cancel our current promise. In that case, the domain
     will clean our continuation with [discontinue_with]. Even if the scheduler
     wants to cancel/clean our continuation, the finaliser [finally] will be
     executed in **any** cases. *)
  let finally () = Atomic.decr domain.syscalls in
  Fun.protect ~finally @@ fun () ->
  Logs.debug (fun m ->
      m "[%a] suspends syscall [%a] (%a)" Domain_uid.pp runner' Syscall_uid.pp
        uid Promise.pp self);
  match Trigger.await trigger with
  | None -> assert (Trigger.is_signaled trigger)
  | Some (exn, bt) ->
      Queue.enqueue domain.cancelled_syscalls uid;
      Printexc.raise_with_backtrace exn bt
  | exception _ -> ()

let signal (Syscall (_uid, trigger, prm)) =
  let runner' = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  if Domain_uid.equal prm.runner runner' = false then
    invalid_arg "This syscall does not belong to the current domain.";
  Signal (trigger, prm)

let uid (Syscall (uid, _, _)) = uid

type 'a orphans = 'a t Sequence.t

let orphans () = Sequence.create ()

let care : type a. a orphans -> a t option option =
 fun orphans ->
  if Sequence.is_empty orphans then None
  else
    let exception Orphan of a t Sequence.node in
    let f ({ Sequence.data= prm; _ } as node) =
      if Computation.is_running prm.state = false then
        raise_notrace (Orphan node)
    in
    try
      Sequence.iter_node ~f orphans;
      Some None
    with Orphan node ->
      let prm = Sequence.data node in
      Sequence.remove node; Some (Some prm)

let quanta =
  match Sys.getenv_opt "MIOU_QUANTA" with
  | Some str -> ( try int_of_string str with _ -> 1)
  | None -> 1

let domains =
  match Sys.getenv_opt "MIOU_DOMAINS" with
  | Some str -> ( try int_of_string str with _ -> Pool.number_of_domains ())
  | None -> Pool.number_of_domains ()

let transfer_dom0_tasks pool =
  if not (Queue.is_empty pool.to_dom0) then
    let elts = Queue.(to_list (transfer pool.to_dom0)) in
    let f = function
      | Dom0_continue { prm; result; k } ->
          let state = State.suspended_with k (Get result) in
          Domain_task (prm, state)
      | Dom0_clean (prm, child) -> Domain_clean (prm, child)
    in
    List.iter (Domain.add_into_domain pool.dom0) (List.map f elts)

let dummy_events = { select= (fun ~block:_ _ -> []); interrupt= Fun.const () }

let run ?(quanta = quanta) ?(g = Random.State.make_self_init ())
    ?(domains = domains) ?(events = Fun.const dummy_events) fn =
  Promise_uid.reset ();
  let dom0 = Domain_uid.of_int 0 in
  let dom0 = Domain.create ~quanta ~events g dom0 in
  let prm0 = Promise.create ~forbid:false dom0.uid in
  Domain.add_into_domain dom0 (Domain_create ([], prm0, fn));
  let pool, domains = Pool.create ~quanta ~dom0 ~domains ~events () in
  let result =
    try
      while Computation.is_running prm0.state && pool.fail = false do
        transfer_dom0_tasks pool; Domain.run pool dom0
      done;
      if not pool.fail then Option.get (Computation.peek prm0.state)
      else Error (Failure "A domain failed", Printexc.get_callstack max_int)
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Error (exn, bt)
  in
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  match result with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let[@tail_mod_cons] rec drop_first_or_not_found x' = function
  | [] -> raise_notrace Not_found
  | x :: xs -> if x == x' then xs else x :: drop_first_or_not_found x' xs

module Mutex = struct
  type 'a value = { trigger: Trigger.t; prm: 'a t }
  type entry = Entry : 'a value -> entry [@@unboxed]

  type state =
    | Unlocked : state
    | Locked : { prm: _ t; head: entry list; tail: entry list } -> state

  let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")
  let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
  let[@inline never] owner () = raise (Sys_error "Mutex: owner")
  let create () = Atomic.make Unlocked

  let rec unlock_as t (self : _ t) backoff =
    match Atomic.get t with
    | Unlocked -> unlocked ()
    | Locked r as seen -> (
        if Promise_uid.equal r.prm.uid self.uid = false then not_owner ()
        else
          match r.head with
          | Entry { trigger; prm } :: rest ->
              let after = Locked { r with prm; head= rest } in
              transfer_as t self seen after trigger backoff
          | [] -> (
              match List.rev r.tail with
              | Entry { trigger; prm } :: rest ->
                  let after = Locked { prm; head= rest; tail= [] } in
                  transfer_as t self seen after trigger backoff
              | [] ->
                  if not (Atomic.compare_and_set t seen Unlocked) then
                    unlock_as t self (Backoff.once backoff)))

  and transfer_as t self seen after trigger backoff =
    if Atomic.compare_and_set t seen after then Trigger.signal trigger
    else unlock_as t self (Backoff.once backoff)

  let unlock t =
    let (Pack self) = Effect.perform Self in
    unlock_as t self Backoff.default

  let rec cleanup_as t (Entry value as entry) backoff =
    match Atomic.get t with
    | Locked r as seen ->
        if Promise_uid.equal r.prm.uid value.prm.uid then
          unlock_as t value.prm backoff
        else if r.head != [] then
          match drop_first_or_not_found entry r.head with
          | head ->
              let after = Locked { r with head } in
              cancel_as t entry seen after backoff
          | exception Not_found ->
              let tail = drop_first_or_not_found entry r.tail in
              let after = Locked { r with tail } in
              cancel_as t entry seen after backoff
        else
          let tail = drop_first_or_not_found entry r.tail in
          let after = Locked { r with tail } in
          cancel_as t entry seen after backoff
    | Unlocked -> unlocked ()

  and cancel_as t entry seen after backoff =
    if not (Atomic.compare_and_set t seen after) then
      cleanup_as t entry (Backoff.once backoff)

  let rec lock_as t prm backoff =
    match Atomic.get t with
    | Unlocked as seen ->
        let after = Locked { prm; head= []; tail= [] } in
        if not (Atomic.compare_and_set t seen after) then
          lock_as t prm (Backoff.once backoff)
    | Locked r as seen ->
        if Promise_uid.equal r.prm.uid prm.uid then owner ()
        else
          let trigger = Trigger.create () in
          let entry = Entry { trigger; prm } in
          let after =
            if r.head == [] then
              Locked { r with head= List.rev_append [ entry ] r.tail; tail= [] }
            else Locked { r with tail= entry :: r.tail }
          in
          if Atomic.compare_and_set t seen after then (
            match Trigger.await trigger with
            | None -> ()
            | Some (exn, bt) ->
                cleanup_as t entry Backoff.default;
                Printexc.raise_with_backtrace exn bt)
          else lock_as t prm (Backoff.once backoff)

  let lock t =
    let (Pack self) = Effect.perform Self in
    lock_as t self Backoff.default

  let try_lock t =
    let (Pack prm) = Effect.perform Self in
    Atomic.get t == Unlocked
    || Atomic.compare_and_set t Unlocked (Locked { prm; head= []; tail= [] })

  let succumb t fn =
    let (Pack prm) = Effect.perform Self in
    unlock_as t prm Backoff.default;
    match fn () with
    | value ->
        Promise.forbid prm (fun () -> lock_as t prm Backoff.default);
        value
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Promise.forbid prm (fun () -> lock_as t prm Backoff.default);
        Printexc.raise_with_backtrace exn bt

  type t = state Atomic.t
end

module Condition = struct
  type state = { head: Trigger.t list; tail: Trigger.t list }

  let empty = { head= []; tail= [] }
  let create () = Atomic.make empty

  let broadcast t =
    if Atomic.get t != empty then begin
      let state = Atomic.exchange t empty in
      List.iter Trigger.signal state.head;
      List.iter Trigger.signal (List.rev state.tail)
    end

  let update_head seen head =
    if head == [] && seen.tail == [] then empty else { seen with head }

  let[@inline always] of_head head =
    if head = [] then empty else { head; tail= [] }

  let[@inline always] of_tail tail =
    if tail = [] then empty else { head= []; tail }

  let rec signal backoff t =
    let seen = Atomic.get t in
    if seen != empty then
      match seen.head with
      | trigger :: head ->
          signal_compare_and_set backoff t seen (update_head seen head) trigger
      | [] -> (
          match List.rev seen.tail with
          | trigger :: head ->
              signal_compare_and_set backoff t seen (of_head head) trigger
          | [] -> assert false)

  and signal_compare_and_set backoff t seen after trigger =
    if Atomic.compare_and_set t seen after then Trigger.signal trigger
    else signal (Backoff.once backoff) t

  let signal t = signal Backoff.default t

  let rec cleanup backoff trigger t =
    let seen = Atomic.get t in
    if seen != empty then
      if seen.head != [] then
        match drop_first_or_not_found trigger seen.head with
        | head ->
            cleanup_compare_and_set backoff trigger t seen
              (update_head seen head)
        | exception Not_found -> (
            match drop_first_or_not_found trigger seen.head with
            | tail ->
                cleanup_compare_and_set backoff trigger t seen
                  { seen with tail }
            | exception Not_found -> signal t)
      else
        match drop_first_or_not_found trigger seen.tail with
        | tail -> cleanup_compare_and_set backoff trigger t seen (of_tail tail)
        | exception Not_found -> signal t

  and cleanup_compare_and_set backoff trigger t seen after =
    if not (Atomic.compare_and_set t seen after) then
      cleanup (Backoff.once backoff) trigger t

  let rec wait backoff trigger t mutex =
    let seen = Atomic.get t in
    let after =
      if seen == empty then { head= [ trigger ]; tail= [] }
      else if seen.head != [] then { seen with tail= trigger :: seen.tail }
      else
        let head = List.rev_append [ trigger ] seen.tail in
        { head; tail= [] }
    in
    if Atomic.compare_and_set t seen after then begin
      match Mutex.succumb mutex @@ fun () -> Trigger.await trigger with
      | None -> ()
      | Some (exn, bt) ->
          cleanup Backoff.default trigger t;
          Printexc.raise_with_backtrace exn bt
    end
    else wait (Backoff.once backoff) trigger t mutex

  let wait t mutex = wait Backoff.default (Trigger.create ()) t mutex

  type t = state Atomic.t
end
