type impossible = |

external reraise : exn -> 'a = "%reraise"

module Queue = Queue
module State = State
module Pqueue = Pqueue
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

let miou_assert = Sys.getenv_opt "MIOU_ASSERT"

let miou_assert : bool -> 'a =
  match miou_assert with
  | None -> fun value -> assert value
  | Some _ ->
      fun value ->
        if not value then begin
          let bt = Printexc.get_callstack max_int in
          Logs.err (fun m ->
              m "[%d] bad assertion at: %s"
                (Stdlib.Domain.self () :> int)
                (Printexc.raw_backtrace_to_string bt));
          assert false
        end
        else assert true

exception Cancelled
exception Still_has_children
exception Not_a_child
exception No_domain_available
exception Not_owner
exception Resource_leaked

let () =
  Printexc.register_printer @@ function
  | Cancelled -> Some "Miou.Cancelled"
  | Still_has_children -> Some "Miou.Still_has_children"
  | Not_a_child -> Some "Miou.Not_a_child"
  | No_domain_available -> Some "Miou.No_domain_available"
  | Not_owner -> Some "Miou.Not_owner"
  | Resource_leaked -> Some "Miou.Resource_leaked"
  | _ -> None

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
  module Uid = Promise_uid

  let create ?parent ~forbid ?resources:(ress = []) runner =
    let resources = Sequence.create () in
    List.iter Sequence.(add Left resources) ress;
    {
      uid= Promise_uid.gen ()
    ; runner
    ; forbid
    ; state= Computation.create ()
    ; parent= Option.map (fun prm -> Pack prm) parent
    ; children= Sequence.create ()
    ; resources
    ; cancelled= None
    ; cleaned= Atomic.make false
    }

  let[@coverage off] pp ppf ({ uid; runner; _ } as prm) =
    Fmt.pf ppf "[%a:%a](%d)" Domain_uid.pp runner Promise_uid.pp uid
      Obj.(reachable_words (repr prm))

  let[@coverage off] pp_pack ppf (Pack prm) = pp ppf prm
  let has_forbidden { forbid; _ } = forbid
  let children_terminated prm = Sequence.is_empty prm.children
  let is_running { state; _ } = Computation.is_running state
  let uid { uid; _ } = uid
  let set t ~forbid = t.forbid <- forbid

  let raise_if_errored t =
    if not t.forbid then Computation.raise_if_errored t.state

  let exchange t ~forbid =
    let seen = t.forbid in
    t.forbid <- forbid;
    seen

  type nonrec 'a t = 'a t
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
  miou_assert (Domain_uid.equal self.runner runner);
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
  | Domain_transfer : _ t * resource * Trigger.t -> domain_elt
  | Domain_create : 'a t * (unit -> 'a) -> domain_elt
  | Domain_cancel : _ t * Printexc.raw_backtrace -> domain_elt
  | Domain_clean : _ t * _ t -> domain_elt
  | Domain_task : 'a t * 'a State.t -> domain_elt
  | Domain_signal : unit t * int * unit State.t -> domain_elt
  | Domain_tick : impossible -> domain_elt

module Domain_elt = struct
  type t = int * domain_elt

  let to_int = function Domain_clean _ -> 1 | Domain_cancel _ -> 2 | _ -> 3

  let compare (t0, a) (t1, b) =
    let value = to_int a - to_int b in
    if value = 0 then Int.compare t0 t1 else value

  let dummy = (0, Domain_tick (Obj.magic ()))
end

module Heapq = Pqueue.Make (Domain_elt)

let promise_of_domain_elt = function
  | Domain_tick _ -> .
  | Domain_create (prm, _) -> Pack prm
  | Domain_cancel (prm, _) -> Pack prm
  | Domain_clean (prm, _) -> Pack prm
  | Domain_task (prm, _) -> Pack prm
  | Domain_transfer (prm, _, _) -> Pack prm
  | Domain_signal (prm, _, _) -> Pack prm

type domain = {
    uid: Domain_uid.t
  ; tasks: Heapq.t
  ; tick: int Atomic.t
  ; quanta: int
  ; g: Random.State.t
  ; events: events
  ; cancelled_syscalls: Syscall_uid.t Queue.t
  ; syscalls: int Atomic.t
  ; hooks: (unit -> unit) Sequence.t
}

type 'r continuation = (State.error option, 'r) State.continuation

type pool_elt =
  | Pool_create : 'a t * (unit -> 'a) -> pool_elt
  | Pool_cancel : _ t * Printexc.raw_backtrace -> pool_elt
  | Pool_clean : _ t * _ t -> pool_elt
  | Pool_continue : {
        prm: 'r t
      ; result: State.error option
      ; k: 'r continuation
    }
      -> pool_elt
  | Pool_transfer : _ t * resource * Trigger.t -> pool_elt

let promise_of_pool_elt = function
  | Pool_create (prm, _) -> Pack prm
  | Pool_cancel (prm, _) -> Pack prm
  | Pool_clean (prm, _) -> Pack prm
  | Pool_continue { prm; _ } -> Pack prm
  | Pool_transfer (prm, _, _) -> Pack prm

type dom0_elt =
  | Dom0_continue : {
        prm: 'r t
      ; result: State.error option
      ; k: 'r continuation
    }
      -> dom0_elt
  | Dom0_clean : _ t * _ t -> dom0_elt
  | Dom0_transfer : _ t * resource * Trigger.t -> dom0_elt

type pool = {
    tasks: pool_elt Sequence.t
  ; mutex: Mutex.t
  ; condition_pending_work: Condition.t
  ; condition_all_idle: Condition.t
  ; domains: domain list
  ; dom0: domain
  ; to_dom0: dom0_elt Queue.t
  ; stop: bool ref
  ; fail: bool ref
  ; working_counter: int ref
  ; domains_counter: int ref
}
(* NOTE(dinosaure): when we create the pool, we do a copy (eg.
   [{ pool with ... }]) to includes spawned domains. To continue sharing mutable
   values when copying, we need to use [ref] rather than [mutable]. *)

let get_domain_from_uid pool ~uid =
  List.find (fun domain -> Domain_uid.equal domain.uid uid) pool.domains

type ty = Concurrent | Parallel of Domain_uid.t
type 'r waiter = { pool: pool; domain: domain; prm: 'r t }

type _ Effect.t +=
  | Spawn : ty * bool * resource list * (unit -> 'a) -> 'a t Effect.t

type _ Effect.t += Self : pack Effect.t
type _ Effect.t += Self_domain : domain Effect.t
type _ Effect.t += Get : 'a -> 'a Effect.t
type _ Effect.t += Random : Random.State.t Effect.t
type _ Effect.t += Domains : Domain_uid.t list Effect.t
type _ Effect.t += Await_cancellation : _ t -> unit Effect.t
type _ Effect.t += Cancel : Printexc.raw_backtrace * 'a t -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Transfer : resource -> Trigger.t Effect.t

let[@coverage off] pp_effect : type a. a Effect.t Fmt.t =
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
  | Trigger.Await _ -> Fmt.string ppf "Await"
  | _ -> Fmt.string ppf "#effect"

let[@coverage off] _pp_domain_elt ppf = function
  | Domain_create (prm, _fn) ->
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
  | Domain_transfer (prm, Resource { uid; _ }, _) ->
      Fmt.pf ppf "@[<1>(Domain_transfer@ %a:%a)@]" Promise.pp prm
        Resource_uid.pp uid
  | Domain_signal (prm, signal, _) ->
      Fmt.pf ppf "@[<1>(Domain_signal@ %a:%d)@]" Promise.pp prm signal
  | Domain_tick _ -> .

let add_into_domain domain elt =
  let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  miou_assert (Domain_uid.equal runner domain.uid);
  let tick = Atomic.fetch_and_add domain.tick 1 in
  Heapq.insert (tick, elt) domain.tasks

let transfer_dom0_tasks pool =
  if not (Queue.is_empty pool.to_dom0) then
    let elts = Queue.(to_list (transfer pool.to_dom0)) in
    let f = function
      | Dom0_continue { prm; result; k } ->
          let state = State.suspended_with k (Get result) in
          Domain_task (prm, state)
      | Dom0_clean (prm, child) -> Domain_clean (prm, child)
      | Dom0_transfer (prm, res, trigger) -> Domain_transfer (prm, res, trigger)
    in
    List.iter (add_into_domain pool.dom0) (List.map f elts)

type signal_retrieved =
  | Signal_retrieved : int * (int -> unit) -> signal_retrieved

let signals = Queue.create ()

let transfer_dom0_signals pool =
  if not (Queue.is_empty signals) then begin
    let elts = Queue.(to_list (transfer signals)) in
    let f (Signal_retrieved (signal, fn)) =
      let prm = Promise.create ~forbid:true pool.dom0.uid in
      let state = State.make fn signal in
      Domain_signal (prm, signal, state)
    in
    Logs.debug (fun m ->
        m "[%a] transfers %d system signal(s)" Domain_uid.pp pool.dom0.uid
          (List.length elts));
    List.iter (add_into_domain pool.dom0) (List.map f elts)
  end

module Domain = struct
  module Uid = Domain_uid

  let create ?(quanta = 1) ~events g uid =
    let tasks = Heapq.create () in
    {
      uid
    ; tasks
    ; tick= Atomic.make 0
    ; quanta
    ; g
    ; events= events uid
    ; syscalls= Atomic.make 0
    ; cancelled_syscalls= Queue.create ()
    ; hooks= Sequence.create ()
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

  let interrupt_parent pool prm =
    match prm.parent with
    | None -> ()
    | Some (Pack parent) -> interrupt pool ~domain:parent.runner

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
  let add_into_domain = add_into_domain

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

  let add_into_dom0 pool elt =
    Queue.enqueue pool.to_dom0 elt;
    pool.dom0.events.interrupt ()

  let cancel pool domain ~backtrace:bt prm =
    (* The promise given does not necessarily belong to the [domain] currently
       in use (the assertion [prm.runner <> domain.uid] exists). However, it is
       impossible for a promise belonging to [dom0] to be executed by another
       domain ([domain.uid <> 0]). We put an [assert false] about such case. *)
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    miou_assert (Domain_uid.equal domain.uid runner);
    Logs.debug (fun m ->
        m "[%a] signals to cancel %a" Domain_uid.pp domain.uid Promise.pp prm);
    match (Domain_uid.to_int prm.runner, Domain_uid.to_int runner) with
    | 0, 0 -> add_into_domain domain (Domain_cancel (prm, bt))
    | 0, _ -> miou_assert false
    | _ -> add_into_pool pool (Pool_cancel (prm, bt))

  let handle pool domain prm state =
    Logs.debug (fun m ->
        m "[%a] handles %a and its continuation %a" Domain_uid.pp domain.uid
          Promise.pp prm State.pp state);
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    miou_assert (Domain_uid.equal runner domain.uid);
    miou_assert (Domain_uid.equal prm.runner domain.uid);
    match state with
    | (State.Suspended _ | State.Unhandled _) as state ->
        add_into_domain domain (Domain_task (prm, state))
    | State.Finished (Error (exn, bt)) ->
        let f (Resource { uid; value; finaliser }) =
          try finaliser value
          with exn ->
            Logs.err (fun m ->
                m "[%a] unexpected exception from the finaliser of [%a](%a): %s"
                  Domain_uid.pp domain.uid Resource_uid.pp uid Promise.pp prm
                  (Printexc.to_string exn))
        in
        Sequence.iter ~f prm.resources;
        Sequence.drop prm.resources;
        let f (Pack prm) = cancel pool domain ~backtrace:bt prm in
        Sequence.iter ~f prm.children;
        ignore (Computation.try_cancel prm.state (exn, bt));
        interrupt_parent pool prm;
        miou_assert (Option.is_some (Computation.cancelled prm.state))
    | State.Finished (Ok value) ->
        if Sequence.is_empty prm.resources = false then begin
          let f (Resource { uid; value; finaliser }) =
            try finaliser value
            with exn ->
              Logs.err (fun m ->
                  m
                    "[%a] unexpected exception from the finaliser of [%a](%a): \
                     %s"
                    Domain_uid.pp domain.uid Resource_uid.pp uid Promise.pp prm
                    (Printexc.to_string exn))
          in
          Sequence.iter ~f prm.resources;
          Sequence.drop prm.resources;
          raise_notrace Resource_leaked
        end;
        if Promise.children_terminated prm = false then
          raise_notrace Still_has_children;
        Logs.debug (fun m ->
            m "[%a] %a finished correctly" Domain_uid.pp domain.uid Promise.pp
              prm);
        interrupt_parent pool prm;
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
        | 0, _ -> miou_assert false
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
    miou_assert
      (Trigger.on_signal trigger (pool, self) child propagate_cancellation);
    miou_assert (Computation.try_attach self.state trigger);
    miou_assert (Computation.try_attach child.state trigger)

  let clean_resources prm resources =
    let to_delete = ref [] in
    let f ({ Sequence.data= Resource { uid; _ }; _ } as node) =
      if
        List.exists
          (fun (Resource { uid= uid'; _ }) -> Resource_uid.equal uid uid')
          resources
      then to_delete := node :: !to_delete
    in
    Sequence.iter_node ~f prm.resources;
    List.iter Sequence.remove !to_delete

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
      | Spawn (Concurrent, forbid, resources, fn) ->
          clean_resources prm resources;
          let prm' = Promise.create ~parent:prm ~forbid ~resources domain.uid in
          canceller pool ~self:prm prm';
          Sequence.(add Left) prm.children (Pack prm');
          add_into_domain domain (Domain_create (prm', fn));
          k (Operation.return prm')
      | Spawn (Parallel runner, forbid, resources, fn) ->
          clean_resources prm resources;
          let prm' = Promise.create ~parent:prm ~forbid ~resources runner in
          canceller pool ~self:prm prm';
          Sequence.(add Left) prm.children (Pack prm');
          add_into_pool pool (Pool_create (prm', fn));
          k (Operation.return prm')
      | Cancel (backtrace, child) ->
          cancel pool domain ~backtrace child;
          k (Operation.continue (Await_cancellation child))
      | Await_cancellation child as await ->
          if
            Promise.is_running child = false
            (* XXX(dinosaure): [&& Sequence.is_empty child.children] is an
               invalid access spotted by TSan. The goal of this check is to be
               sure that children of our [child] are cancelled too. However,
               [child.cleaned] should be enough. *)
            && Atomic.get child.cleaned
          then k (Operation.return (clean_children ~self:prm child))
          else k (Operation.continue await)
      | Transfer res ->
          let (Pack parent) = Option.get prm.parent in
          let trigger = Trigger.create () in
          if Domain_uid.to_int parent.runner = 0 then
            add_into_dom0 pool (Dom0_transfer (parent, res, trigger))
          else add_into_pool pool (Pool_transfer (parent, res, trigger));
          k (Operation.return trigger)
      | Trigger.Await _ -> k Operation.interrupt
      | Yield -> k Operation.yield
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
    miou_assert (Domain_uid.equal runner domain.uid);
    miou_assert (Domain_uid.equal prm.runner domain.uid);
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
      | _, Domain_tick _ -> .
      | _, Domain_cancel _ | _, Domain_clean _ | _, Domain_signal _ -> ()
      | _, elt ->
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
        miou_assert (Domain_uid.equal prm.runner runner);
        miou_assert (Domain_uid.equal domain.uid runner);
        match (Domain_uid.to_int parent.runner, Domain_uid.to_int runner) with
        | 0, 0 -> add_into_domain domain (Domain_clean (parent, prm))
        | 0, _ -> add_into_dom0 pool (Dom0_clean (parent, prm))
        | _, 0 -> miou_assert false
        | _ -> add_into_pool pool (Pool_clean (parent, prm)))

  let handle_signal ~signal domain prm state =
    Logs.debug (fun m ->
        m "[%a] handles signal (%d) %a and its continuation %a" Domain_uid.pp
          domain.uid signal Promise.pp prm State.pp state);
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    miou_assert (Domain_uid.equal runner domain.uid);
    miou_assert (Domain_uid.equal prm.runner domain.uid);
    match state with
    | (State.Suspended _ | State.Unhandled _) as state ->
        add_into_domain domain (Domain_signal (prm, signal, state))
    | State.Finished (Error (exn, _bt)) ->
        Logs.err (fun m ->
            m "[%a] a signal handler raised an exception: %s" Domain_uid.pp
              domain.uid (Printexc.to_string exn))
    | State.Finished (Ok ()) -> ()

  let once pool domain = function
    | Domain_tick _ -> .
    | Domain_create (prm, fn) -> (
        match Computation.cancelled prm.state with
        | None ->
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
    | Domain_signal (prm, signal, State.Suspended (k, Trigger.Await trigger)) ->
        Logs.debug (fun m ->
            m "[%a] %a await (signaled by %d)" Domain_uid.pp domain.uid
              Promise.pp prm signal);
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
            if Sequence.is_empty prm.children then begin
              Atomic.set prm.cleaned true;
              handle pool domain prm state
            end
            else add_into_domain domain (Domain_task (prm, state)))
    | Domain_clean (prm, child) -> clean_children ~self:prm child
    | Domain_transfer (prm, res, trigger) ->
        Sequence.(add Left) prm.resources res;
        Trigger.signal trigger
    | Domain_signal (prm, signal, state) -> (
        miou_assert (Domain_uid.equal prm.runner (Domain_uid.of_int 0));
        miou_assert (Domain_uid.equal domain.uid (Domain_uid.of_int 0));
        let perform = perform pool domain prm in
        match Computation.cancelled prm.state with
        | None ->
            let state = State.run ~quanta:domain.quanta ~perform state in
            handle_signal ~signal domain prm state
        | Some (exn, bt) ->
            let state = State.fail ~backtrace:bt ~exn state in
            handle_signal ~signal domain prm state)
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
          | Some (Domain_create (prm', _)) ->
              miou_assert (Promise_uid.equal prm.uid prm'.uid);
              miou_assert (Domain_uid.equal prm.runner domain.uid);
              let state = State.pure (Error (Cancelled, bt)) in
              handle pool domain prm state
          | Some (Domain_task (prm', state)) ->
              miou_assert (Promise_uid.equal prm.uid prm'.uid);
              miou_assert (Domain_uid.equal prm.runner domain.uid);
              let state = State.fail ~backtrace:bt ~exn:Cancelled state in
              handle pool domain prm' state
          | Some (Domain_cancel _)
          | Some (Domain_clean _)
          | Some (Domain_transfer _)
          | Some (Domain_signal _)
          | None ->
              ()
        in
        Logs.debug (fun m ->
            m "[%a] clean-up children (%a) of %a" Domain_uid.pp domain.uid
              Promise.pp prm
              Fmt.(Dump.option Promise.pp_pack)
              prm.parent);
        if Sequence.is_empty prm.children then begin
          signal_to_clean_children pool domain prm;
          Atomic.set prm.cleaned true
        end
        else add_into_domain domain cancellation

  let signal_system_events domain (Signal (trigger, prm)) =
    let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
    miou_assert (Domain_uid.equal runner domain.uid);
    miou_assert (Domain_uid.equal runner prm.runner);
    Logs.debug (fun m ->
        m "[%a] signals syscall into %a" Domain_uid.pp runner Promise.pp prm);
    try Trigger.signal trigger
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      (* TODO(dinosaure): we actually loose [exn] (and replace it then by
         [Cancelled]). *)
      ignore (Computation.try_cancel prm.state (exn, bt));
      add_into_domain domain (Domain_cancel (prm, bt))

  let synchronize_dom0_tasks pool =
    transfer_dom0_tasks pool; transfer_dom0_signals pool

  let unblock_awaits_with_system_events pool (domain : domain) =
    if (Stdlib.Domain.self () :> int) == 0 then synchronize_dom0_tasks pool;
    let block = Heapq.size domain.tasks = 0 in
    let cancelled = Queue.(to_list (transfer domain.cancelled_syscalls)) in
    let syscalls = domain.events.select ~block cancelled in
    Logs.debug (fun m ->
        m "[%a] handles %d signal(s)" Domain_uid.pp domain.uid
          (List.length syscalls));
    List.iter (signal_system_events domain) syscalls

  let system_events_suspended domain = Atomic.get domain.syscalls > 0

  let run_hooks domain =
    let apply ({ Sequence.data= fn; _ } as node) =
      try fn ()
      with exn ->
        Logs.err (fun m ->
            m "[%a] a hook raised an exception: %s" Domain_uid.pp domain.uid
              (Printexc.to_string exn));
        Sequence.remove node
    in
    Sequence.iter_node ~f:apply domain.hooks

  let run pool (domain : domain) =
    run_hooks domain;
    match Heapq.extract_min_exn domain.tasks with
    | exception Heapq.Empty ->
        if system_events_suspended domain then
          unblock_awaits_with_system_events pool domain
    | _, elt ->
        Logs.debug (fun m ->
            m "[%a] does %a" Domain_uid.pp domain.uid _pp_domain_elt elt);
        once pool domain elt;
        if system_events_suspended domain then
          unblock_awaits_with_system_events pool domain

  let self () =
    let { uid; _ } = Effect.perform Self_domain in
    uid

  let available () = List.length (Effect.perform Domains)
end

module Clatch = struct
  type t = { mutex: Mutex.t; condition: Condition.t; mutable count: int }

  let create n =
    { mutex= Mutex.create (); condition= Condition.create (); count= n }

  let await t =
    let finally () = Mutex.unlock t.mutex in
    Mutex.lock t.mutex;
    Fun.protect ~finally @@ fun () ->
    while t.count > 0 do
      Condition.wait t.condition t.mutex
    done

  let count_down t =
    let finally () = Mutex.unlock t.mutex in
    Mutex.lock t.mutex;
    Fun.protect ~finally @@ fun () ->
    t.count <- t.count - 1;
    Condition.broadcast t.condition
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
      | Pool_create (prm, fn) -> Domain_create (prm, fn)
      | Pool_cancel (prm, bt) -> Domain_cancel (prm, bt)
      | Pool_clean (prm, child) -> Domain_clean (prm, child)
      | Pool_transfer (prm, res, trigger) -> Domain_transfer (prm, res, trigger)
      | Pool_continue { prm; result; k } ->
          let state = State.suspended_with k (Get result) in
          Domain_task (prm, state)
    in
    let elts = List.map f !nodes in
    List.iter (Domain.add_into_domain domain) elts

  let worker pool domain =
    let exception Exit in
    try
      while true do
        Mutex.lock pool.mutex;
        while nothing_to_do pool domain && not !(pool.stop) do
          Condition.wait pool.condition_pending_work pool.mutex
        done;
        if !(pool.stop) then raise_notrace Exit;
        transfer_all_tasks pool domain;
        incr pool.working_counter;
        Mutex.unlock pool.mutex;
        Domain.run pool domain;
        Mutex.lock pool.mutex;
        decr pool.working_counter;
        if (not !(pool.stop)) && Int.equal !(pool.working_counter) 0 then
          Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex
      done
    with
    | Exit ->
        Logs.debug (fun m -> m "[%a] exits" Domain_uid.pp domain.uid);
        decr pool.domains_counter;
        Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex
    | exn ->
        Mutex.lock pool.mutex;
        pool.stop := true;
        pool.fail := true;
        decr pool.domains_counter;
        Condition.broadcast pool.condition_pending_work;
        Condition.signal pool.condition_all_idle;
        Mutex.unlock pool.mutex;
        reraise exn

  let wait pool =
    let exception Exit in
    try
      let working_domains () =
        !(pool.working_counter) > 0 && not !(pool.stop)
      in
      let all_domains () = !(pool.stop) && !(pool.domains_counter) > 0 in
      while true do
        Mutex.lock pool.mutex;
        if working_domains () || all_domains () then begin
          Condition.wait pool.condition_all_idle pool.mutex;
          Mutex.unlock pool.mutex
        end
        else raise_notrace Exit
      done
    with Exit -> Mutex.unlock pool.mutex

  let kill pool =
    Mutex.lock pool.mutex;
    pool.stop := true;
    Logs.debug (fun m -> m "[%a] kills domain(s)" Domain_uid.pp pool.dom0.uid);
    Condition.broadcast pool.condition_pending_work;
    Mutex.unlock pool.mutex;
    wait pool

  let number_of_domains () =
    max 0 (Stdlib.Domain.recommended_domain_count () - 1)

  let create ?(quanta = 1) ~dom0
      ?domains:(domains_counter = number_of_domains ()) ~events () =
    let pool =
      {
        tasks= Sequence.create ()
      ; mutex= Mutex.create ()
      ; condition_pending_work= Condition.create ()
      ; condition_all_idle= Condition.create ()
      ; stop= ref false
      ; fail= ref false
      ; working_counter= ref 0
      ; domains_counter= ref domains_counter
      ; domains= []
      ; dom0
      ; to_dom0= Queue.create ()
      }
    in
    let clatch = Clatch.create domains_counter in
    let domains = Queue.create () in
    let spawn () =
      Stdlib.Domain.spawn @@ fun () ->
      let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
      let domain = Domain.create ~quanta ~events dom0.g runner in
      Queue.enqueue domains domain;
      Logs.debug (fun m -> m "spawn the domain [%a]" Domain_uid.pp runner);
      Clatch.count_down clatch;
      Clatch.await clatch;
      worker { pool with domains= Queue.to_list domains } domain
    in
    let vs = List.init domains_counter (Fun.const ()) in
    let vs = List.map spawn vs in
    Clatch.await clatch;
    ({ pool with domains= Queue.to_list domains }, vs)
end

module Ownership = struct
  type t = resource

  let create ~finally:finaliser value =
    Resource { uid= Resource_uid.gen (); value; finaliser }

  let check (Resource { uid; _ }) =
    let (Pack self) = Effect.perform Self in
    Logs.debug (fun m ->
        m "[%a] checks if [%a] is owned by %a" Domain_uid.pp self.runner
          Resource_uid.pp uid Promise.pp self);
    let equal (Resource { uid= uid'; _ }) = Resource_uid.equal uid uid' in
    if Sequence.exists equal self.resources = false then raise Not_owner

  let own (Resource { uid; _ } as res) =
    let (Pack self) = Effect.perform Self in
    Logs.debug (fun m ->
        m "[%a] adds [%a] into %a" Domain_uid.pp self.runner Resource_uid.pp uid
          Promise.pp self);
    let equal (Resource { uid= uid'; _ }) = Resource_uid.equal uid uid' in
    if Sequence.exists equal self.resources then
      invalid_arg "The current promise already holds the resource given"
    else Sequence.(add Left) self.resources res

  exception Found_resource of resource Sequence.node

  let disown (Resource { uid; _ }) =
    let (Pack self) = Effect.perform Self in
    let equal (Resource { uid= uid'; _ }) = Resource_uid.equal uid uid' in
    try
      let f ({ Sequence.data; _ } as node) =
        if equal data then raise_notrace (Found_resource node)
      in
      Sequence.iter_node ~f self.resources;
      raise Not_owner
    with Found_resource node -> Sequence.remove node

  let transfer (Resource { uid; _ } as res) =
    let (Pack self) = Effect.perform Self in
    if Option.is_none self.parent then
      invalid_arg
        "The current promise has no parent, so it is impossible to transfer a \
         resource";
    let equal (Resource { uid= uid'; _ }) = Resource_uid.equal uid uid' in
    try
      let f ({ Sequence.data; _ } as node) =
        if equal data then raise_notrace (Found_resource node)
      in
      Sequence.iter_node ~f self.resources;
      raise Not_owner
    with Found_resource node -> (
      Logs.debug (fun m ->
          m "[%a] transfers the resource [%a]" Domain_uid.pp self.runner
            Resource_uid.pp uid);
      let (Pack parent) = Option.get self.parent in
      if Domain_uid.equal self.runner parent.runner then
        let () = Sequence.(add Left) parent.resources res in
        Sequence.remove node
      else
        let trigger = Effect.perform (Transfer res) in
        match Trigger.await trigger with
        | None -> Sequence.remove node
        | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
        | exception _ -> Sequence.remove node)
end

let call_cc ?(give = []) ?orphans fn =
  let prm = Effect.perform (Spawn (Concurrent, false, give, fn)) in
  Logs.debug (fun m -> m "%a spawned" Promise.pp prm);
  Option.iter (fun s -> Sequence.(add Left) s prm) orphans;
  prm

let call ?(give = []) ?orphans fn =
  let domains = Effect.perform Domains in
  if domains = [] then raise No_domain_available;
  let (Pack self) = Effect.perform Self in
  let g = Effect.perform Random in
  let runner =
    match List.filter (Fun.negate (Domain_uid.equal self.runner)) domains with
    | [] -> raise No_domain_available
    | lst -> List.nth lst (Random.State.int g (List.length lst))
  in
  let prm = Effect.perform (Spawn (Parallel runner, false, give, fn)) in
  Option.iter (fun s -> Sequence.(add Left) s prm) orphans;
  prm

let await prm =
  let (Pack self) = Effect.perform Self in
  Logs.debug (fun m -> m "%a await %a" Promise.pp self Promise.pp prm);
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
  let g = Effect.perform Random in
  let some (Pack parent) = Promise_uid.equal self.uid parent.uid in
  let is_a_child prm = Option.fold ~none:false ~some prm.parent in
  if not (List.for_all is_a_child prms) then raise_notrace Not_a_child;
  let c = Computation.create () in
  let choose =
    Computation.try_capture c @@ fun () ->
    let t = Trigger.create () in
    let take_one_and_detach_rest unattached attached =
      List.iter (fun prm -> Computation.detach prm.state t) attached;
      let _, terminated =
        List.partition Promise.is_running (attached @ unattached)
      in
      let filter prm =
        if Result.is_ok (Option.get (Computation.peek prm.state)) then
          Either.Left prm
        else Either.Right prm
      in
      let result, prm =
        match List.partition_map filter terminated with
        | [], prms | prms, _ ->
            let n = Random.State.int g (List.length prms) in
            let prm = List.nth prms n in
            (Computation.await prm.state, prm)
      in
      clean_children ~self prm; result
    in
    let rec try_attach_all attached = function
      | prm :: prms ->
          let attached = prm :: attached in
          if Computation.try_attach prm.state t then
            try_attach_all attached prms
          else take_one_and_detach_rest prms attached
      | [] -> (
          match Trigger.await t with
          | Some (exn, bt) ->
              List.iter (fun prm -> Computation.detach prm.state t) attached;
              Error (exn, bt)
          | None -> take_one_and_detach_rest [] prms)
    in
    try_attach_all [] prms
  in
  let prm = call_cc choose in
  miou_assert (await_exn prm);
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
  let g = Effect.perform Random in
  let some (Pack parent) = Promise_uid.equal self.uid parent.uid in
  let is_a_child prm = Option.fold ~none:false ~some prm.parent in
  let bt = Printexc.get_callstack max_int in
  if not (List.for_all is_a_child prms) then raise_notrace Not_a_child;
  let c = Computation.create () in
  let choose =
    Computation.try_capture c @@ fun () ->
    let t = Trigger.create () in
    let take_one_and_cancel_rest unattached attached =
      List.iter (fun prm -> Computation.detach prm.state t) attached;
      let in_progress, terminated =
        List.partition Promise.is_running (attached @ unattached)
      in
      List.iter (cancel ~self ~backtrace:bt) in_progress;
      let filter prm =
        if Result.is_ok (Option.get (Computation.peek prm.state)) then
          Either.Left prm
        else Either.Right prm
      in
      let result, prm =
        match List.partition_map filter terminated with
        | [], prms ->
            Logs.debug (fun m ->
                m "[%a] only cancelled tasks are done" Domain_uid.pp self.runner);
            let n = Random.State.int g (List.length prms) in
            let prm = List.nth prms n in
            (Computation.await prm.state, prm)
        | prms, _ ->
            Logs.debug (fun m ->
                m "[%a] few tasks are completed" Domain_uid.pp self.runner);
            let n = Random.State.int g (List.length prms) in
            let prm = List.nth prms n in
            (Computation.await prm.state, prm)
      in
      let exclude (prm' : _ t) =
        if Promise_uid.equal prm.uid prm'.uid = false then
          cancel ~self ~backtrace:bt prm'
      in
      List.iter exclude terminated;
      clean_children ~self prm;
      result
    in
    let rec try_attach_all attached = function
      | prm :: prms ->
          let attached = prm :: attached in
          if Computation.try_attach prm.state t then
            try_attach_all attached prms
          else take_one_and_cancel_rest prms attached
      | [] -> (
          match Trigger.await t with
          | Some (exn, bt) ->
              List.iter (fun prm -> Computation.detach prm.state t) attached;
              Error (exn, bt)
          | None ->
              Logs.debug (fun m ->
                  m "[%a] one promise finished" Promise.pp self);
              take_one_and_cancel_rest [] prms)
    in
    try_attach_all [] prms
  in
  let prm = call_cc choose in
  miou_assert (await_exn prm);
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
    Effect.perform (Spawn (Parallel runner, false, [], fn))
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

let syscall () =
  let uid = Syscall_uid.gen () in
  let trigger = Trigger.create () in
  let (Pack self) = Effect.perform Self in
  let runner = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  let domain = Effect.perform Self_domain in
  miou_assert (Domain_uid.equal runner domain.uid);
  miou_assert (Domain_uid.equal runner self.runner);
  Syscall (uid, trigger, self)

let suspend (Syscall (uid, trigger, prm)) =
  let domain = Effect.perform Self_domain in
  let (Pack self) = Effect.perform Self in
  if Promise_uid.equal self.uid prm.uid = false then
    invalid_arg "This syscall does not belong to the current promise";
  let runner' = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  miou_assert (Domain_uid.equal domain.uid runner');
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
  | None -> miou_assert (Trigger.is_signaled trigger)
  | Some (exn, bt) ->
      Queue.enqueue domain.cancelled_syscalls uid;
      Printexc.raise_with_backtrace exn bt
  | exception _ -> Queue.enqueue domain.cancelled_syscalls uid

let signal (Syscall (_uid, trigger, prm)) =
  let runner' = Domain_uid.of_int (Stdlib.Domain.self () :> int) in
  if Domain_uid.equal prm.runner runner' = false then
    invalid_arg "This syscall does not belong to the current domain.";
  Signal (trigger, prm)

let uid (Syscall (uid, _, _)) = uid

type 'a orphans = 'a t Sequence.t

let orphans () = Sequence.create ()
let length = Sequence.length

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

module Hook = struct
  type t = { uid: Domain.Uid.t; node: (unit -> unit) Sequence.node }

  let add fn =
    let domain = Effect.perform Self_domain in
    Sequence.(add Left) domain.hooks fn;
    let node = Sequence.(peek_node Left) domain.hooks in
    { uid= domain.uid; node }

  let remove hook =
    let domain = Effect.perform Self_domain in
    if Domain.Uid.equal domain.uid hook.uid then Sequence.remove hook.node
    else invalid_arg "The hook does not belong into the current domain"
end

let quanta =
  match Sys.getenv_opt "MIOU_QUANTA" with
  | Some str -> ( try int_of_string str with _ -> 1)
  | None -> 1

let domains =
  match Sys.getenv_opt "MIOU_DOMAINS" with
  | Some str -> ( try int_of_string str with _ -> Pool.number_of_domains ())
  | None -> Pool.number_of_domains ()

let error_select =
  "Your program is waiting for a system event when you are using Miou.run \
   (which does not handle system events). You should use Miou_unix.run if you \
   use functions proposed by the Miou_unix module or use your own run function \
   associated with your suspensions."

exception No_select_provided

let () =
  Printexc.register_printer @@ function
  | No_select_provided -> Some error_select
  | _ -> None

let dummy_events =
  let select ~block:_ _ = raise No_select_provided in
  { select; interrupt= Fun.const () }

let sys_signal signal = function
  | (Sys.Signal_default | Sys.Signal_ignore) as behavior ->
      Sys.signal signal behavior
  | Sys.Signal_handle fn ->
      let fn signal =
        Logs.debug (fun m ->
            m "[%d] got a signal %d" (Stdlib.Domain.self () :> int) signal);
        Queue.enqueue signals (Signal_retrieved (signal, fn))
      in
      Sys.signal signal (Signal_handle fn)

let run ?(quanta = quanta) ?(g = Random.State.make_self_init ())
    ?(domains = domains) ?(events = Fun.const dummy_events) fn =
  Promise_uid.reset ();
  let dom0 = Domain_uid.of_int 0 in
  let dom0 = Domain.create ~quanta ~events g dom0 in
  let prm0 = Promise.create ~forbid:false dom0.uid in
  Domain.add_into_domain dom0 (Domain_create (prm0, fn));
  let pool, domains = Pool.create ~quanta ~dom0 ~domains ~events () in
  let result =
    try
      while Computation.is_running prm0.state && !(pool.fail) = false do
        transfer_dom0_tasks pool;
        transfer_dom0_signals pool;
        Domain.run pool dom0
      done;
      if not !(pool.fail) then Option.get (Computation.peek prm0.state)
      else Error (Failure "A domain failed", Printexc.get_callstack max_int)
    with exn ->
      Logs.err (fun m ->
          m "[%a] failed with %S" Domain_uid.pp dom0.uid
            (Printexc.to_string exn));
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

(* NOTE(dinosaure): about Mutex and Condition, we must clean these resources,
   especially when the cancellation operates. Miou offers a last shot when a
   task is cancelled to clean everything. We provide something like
   [Fun.protect] to trigger the cancellation and run a last function which
   should clean everything. [finally] operates in any case and informs us if the
   task was cancelled or not. [on_cancellation] operates only if the task is
   cancelled. This is where we should clean everything.

   [finally] and [on_cancellation] should not perform an effect when the
   cancellation operates. An effect suspends the task and Miou will not give you
   an opportunity to continue (because we want to delete the task). Also,
   [finally] and [on_cancellation] should not raise an exception. As
   [Fun.protect], [Finally_raised] is used instead of. *)
exception On_cancellation_raised of exn

let () =
  Printexc.register_printer @@ function
  | On_cancellation_raised exn ->
      Some ("Miou.On_cancellation_raised: " ^ Printexc.to_string exn)
  | _ -> None

let protect ~on_cancellation ~finally fn =
  let finally_no_exn ~cancelled =
    try finally ~cancelled
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Fun.Finally_raised exn) bt
  in
  let on_cancellation_no_exn () =
    try on_cancellation ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (On_cancellation_raised exn) bt
  in
  match fn () with
  | result ->
      finally_no_exn ~cancelled:false;
      result
  | exception Cancelled ->
      let bt = Printexc.get_raw_backtrace () in
      finally_no_exn ~cancelled:true;
      on_cancellation_no_exn ();
      Printexc.raise_with_backtrace Cancelled bt
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      finally_no_exn ~cancelled:false;
      Printexc.raise_with_backtrace exn bt

module Mutex = struct
  type 'a value = { trigger: Trigger.t; prm: 'a t option }
  type entry = Entry : 'a value -> entry [@@unboxed]

  type state =
    | Unlocked
    | Locked : { prm: _ t option; head: entry list; tail: entry list } -> state

  let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")
  let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
  let[@inline never] owner () = raise (Sys_error "Mutex: owner")
  let create () = Atomic.make Unlocked
  let locked_nothing = Locked { prm= None; head= []; tail= [] }

  let rec unlock_as t (self : _ t option) backoff =
    match Atomic.get t with
    | Unlocked -> unlocked ()
    | Locked r as seen ->
        let is_owner =
          match (self, r.prm) with
          | None, _ | _, None -> true
          | Some a, Some b -> Promise_uid.equal a.uid b.uid
        in
        if is_owner then begin
          match r.head with
          | Entry { trigger; prm } :: rest ->
              let after = Locked { r with prm; head= rest } in
              transfer_as t self seen after trigger backoff
          | [] -> begin
              match List.rev r.tail with
              | Entry { trigger; prm } :: rest ->
                  let after = Locked { prm; head= rest; tail= [] } in
                  transfer_as t self seen after trigger backoff
              | [] ->
                  if not (Atomic.compare_and_set t seen Unlocked) then
                    unlock_as t self (Backoff.once backoff)
            end
        end
        else not_owner ()

  and transfer_as t self seen after trigger backoff =
    if Atomic.compare_and_set t seen after then Trigger.signal trigger
    else unlock_as t self (Backoff.once backoff)

  let[@inline] unlock t =
    try
      let (Pack self) = Effect.perform Self in
      unlock_as t (Some self) Backoff.default
    with Effect.Unhandled Self -> unlock_as t None Backoff.default

  let rec cleanup_as t (Entry value as entry) backoff =
    match Atomic.get t with
    | Locked r as seen ->
        let is_equal =
          match (r.prm, value.prm) with
          | None, None -> true
          | Some a, Some b -> Promise_uid.equal a.uid b.uid
          | _ -> false
        in
        if is_equal then unlock_as t value.prm backoff
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

  let rec lock_as t self backoff =
    match Atomic.get t with
    | Unlocked as seen ->
        let after =
          match self with
          | None -> locked_nothing
          | Some _ -> Locked { prm= self; head= []; tail= [] }
        in
        if not (Atomic.compare_and_set t seen after) then
          lock_as t self (Backoff.once backoff)
    | Locked r as seen ->
        let trigger = Trigger.create () in
        let entry =
          match (self, r.prm) with
          | _, None -> Entry { trigger; prm= None }
          | None, Some prm ->
              let (Pack self) = Effect.perform Self in
              if Promise_uid.equal self.uid prm.uid = false then
                Entry { trigger; prm= Some self }
              else owner ()
          | Some self, Some prm ->
              if Promise_uid.equal self.uid prm.uid = false then
                Entry { trigger; prm= Some self }
              else owner ()
        in
        let after =
          if r.head == [] then
            Locked { r with head= List.rev_append r.tail [ entry ]; tail= [] }
          else Locked { r with tail= entry :: r.tail }
        in
        if Atomic.compare_and_set t seen after then begin
          let on_cancellation () = cleanup_as t entry Backoff.default in
          let finally ~cancelled:_ = () in
          protect ~on_cancellation ~finally @@ fun () ->
          ignore (Trigger.await trigger)
        end
        else lock_as t self (Backoff.once backoff)

  let[@inline] lock t =
    try
      let (Pack self) = Effect.perform Self in
      lock_as t (Some self) Backoff.default
    with Effect.Unhandled Self -> lock_as t None Backoff.default

  let try_lock t =
    try
      let (Pack prm) = Effect.perform Self in
      Atomic.get t == Unlocked
      || Atomic.compare_and_set t Unlocked
           (Locked { prm= Some prm; head= []; tail= [] })
    with Effect.Unhandled Self ->
      Atomic.get t == Unlocked
      || Atomic.compare_and_set t Unlocked locked_nothing

  let inhibit fn = try fn () with _ -> ()

  let protect t fn =
    try
      let (Pack self) = Effect.perform Self in
      lock_as t (Some self) Backoff.default;
      match fn () with
      | value ->
          unlock_as t (Some self) Backoff.default;
          value
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          (* NOTE(dinosaure): we must inhibit exceptions from [unlock_as]. In
             the cancellation case, [fn] didn't probably re-lock the mutex (and
             that's what's intended). So, [unlock_as] can raise another
             exception [Sys_error "Mutex: unlocked"] but [exn] is more important
             to reraise. *)
          inhibit (fun () -> unlock_as t (Some self) Backoff.default);
          Printexc.raise_with_backtrace exn bt
    with Effect.Unhandled Self -> (
      lock_as t None Backoff.default;
      match fn () with
      | value ->
          unlock_as t None Backoff.default;
          value
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          inhibit (fun () -> unlock_as t None Backoff.default);
          Printexc.raise_with_backtrace exn bt)

  type t = state Atomic.t
end

module Condition = struct
  type queue = { head: Trigger.t list; tail: Trigger.t list }
  type state = Empty | Queue of queue

  let create () = Atomic.make Empty

  let broadcast t =
    if Atomic.get t != Empty then begin
      match Atomic.exchange t Empty with
      | Empty -> ()
      | Queue state ->
          List.iter Trigger.signal state.head;
          List.iter Trigger.signal (List.rev state.tail)
    end

  let[@inline always] update_head seen head =
    if head == [] && seen.tail == [] then Empty else Queue { seen with head }

  let[@inline always] of_head head =
    if head = [] then Empty else Queue { head; tail= [] }

  let[@inline always] of_tail tail =
    if tail = [] then Empty else Queue { head= []; tail }

  let rec signal backoff t =
    match Atomic.get t with
    | Empty -> ()
    | Queue state as seen -> (
        match state.head with
        | trigger :: head ->
            signal_compare_and_set backoff t seen (update_head state head)
              trigger
        | [] -> (
            match List.rev state.tail with
            | trigger :: head ->
                signal_compare_and_set backoff t seen (of_head head) trigger
            | [] -> assert false))

  and signal_compare_and_set backoff t seen after trigger =
    if Atomic.compare_and_set t seen after then Trigger.signal trigger
    else signal (Backoff.once backoff) t

  let signal t = signal Backoff.default t

  let rec cleanup backoff trigger t =
    match Atomic.get t with
    | Empty -> ()
    | Queue state as seen -> (
        if state.head != [] then
          match drop_first_or_not_found trigger state.head with
          | head ->
              cleanup_compare_and_set backoff trigger t seen
                (update_head state head)
          | exception Not_found -> (
              match drop_first_or_not_found trigger state.tail with
              | tail ->
                  cleanup_compare_and_set backoff trigger t seen
                    (Queue { state with tail })
              | exception Not_found -> signal t)
        else
          match drop_first_or_not_found trigger state.tail with
          | tail ->
              cleanup_compare_and_set backoff trigger t seen (of_tail tail)
          | exception Not_found -> signal t)

  and cleanup_compare_and_set backoff trigger t seen after =
    if not (Atomic.compare_and_set t seen after) then
      cleanup (Backoff.once backoff) trigger t

  let rec wait backoff prm trigger t mutex =
    let seen = Atomic.get t in
    let after =
      match seen with
      | Empty -> Queue { head= [ trigger ]; tail= [] }
      | Queue state ->
          if state.head != [] then
            Queue { state with tail= trigger :: state.tail }
          else Queue { head= List.rev_append state.tail [ trigger ]; tail= [] }
    in
    if Atomic.compare_and_set t seen after then begin
      let on_cancellation () = cleanup Backoff.default trigger t in
      let finally ~cancelled:_ = () in
      Mutex.unlock_as mutex (Some prm) Backoff.default;
      protect ~finally ~on_cancellation @@ fun () ->
      let _result = Trigger.await trigger in
      let forbid = Promise.exchange prm ~forbid:true in
      Mutex.lock_as mutex (Some prm) Backoff.default;
      Promise.set prm ~forbid
    end
    else wait (Backoff.once backoff) prm trigger t mutex

  let wait t mutex =
    let trigger = Trigger.create () in
    let (Pack self) = Effect.perform Self in
    wait Backoff.default self trigger t mutex

  type t = state Atomic.t
end

module Lazy = struct
  exception Undefined = Stdlib.Lazy.Undefined

  type 'a state =
    | Fun of (unit -> 'a)
    | Run : { prm: _ t; triggers: Trigger.t list } -> 'a state
    | Val of 'a
    | Exn of { exn: exn; trace: Printexc.raw_backtrace }

  type 'a t = 'a state Atomic.t

  let from_val v = Atomic.make (Val v)
  let from_fun fn = Atomic.make (Fun fn)

  let rec cleanup t trigger backoff =
    match Atomic.get t with
    | Val _ | Exn _ -> ()
    | Fun _ -> failwith "impossible"
    | Run r as seen -> (
        match drop_first_or_not_found trigger r.triggers with
        | triggers ->
            let after = Run { r with triggers } in
            if not (Atomic.compare_and_set t seen after) then
              cleanup t trigger (Backoff.once backoff)
        | exception Not_found -> ())

  let rec force : type a b. a t -> b Promise.t -> Backoff.t -> a =
   fun t prm backoff ->
    match Atomic.get t with
    | Val v -> v
    | Exn r -> Printexc.raise_with_backtrace r.exn r.trace
    | Fun fn as seen ->
        let after = Run { prm; triggers= [] } in
        if Atomic.compare_and_set t seen after then begin
          let result =
            match fn () with
            | v -> Val v
            | exception exn ->
                let trace = Printexc.get_raw_backtrace () in
                Exn { exn; trace }
          in
          match Atomic.exchange t result with
          | Val _ | Exn _ | Fun _ -> failwith "impossible"
          | Run r ->
              List.iter Trigger.signal r.triggers;
              force t prm Backoff.default
        end
        else force t prm (Backoff.once backoff)
    | Run r as seen ->
        if Promise.Uid.equal r.prm.uid prm.uid then raise Undefined
        else
          let trigger = Trigger.create () in
          let triggers = trigger :: r.triggers in
          let after = Run { r with triggers } in
          if Atomic.compare_and_set t seen after then begin
            let on_cancellation () = cleanup t trigger Backoff.default in
            let finally ~cancelled:_ = () in
            protect ~on_cancellation ~finally @@ fun () ->
            ignore (Trigger.await trigger);
            force t prm Backoff.default
          end
          else force t prm (Backoff.once backoff)

  let force t =
    match Atomic.get t with
    | Val v -> v
    | Exn r -> Printexc.raise_with_backtrace r.exn r.trace
    | Fun _ | Run _ ->
        let (Pack self) = Effect.perform Self in
        Promise.raise_if_errored self;
        force t self Backoff.default
end

module Sequence = struct
  include Sequence

  let add direction t value = add direction t value; peek_node direction t
end
