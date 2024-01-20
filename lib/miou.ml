external apply : ('a -> 'b) -> 'a -> 'b = "%apply"
external reraise : exn -> 'a = "%reraise"

module Queue = Queue
module State = State
module Logs = Logs
module Heapq = Heapq
module Fmt = Fmt

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
    let key = Stdlib.Domain.DLS.new_key @@ fun () -> null + 1 in
    fun () ->
      let ret = Stdlib.Domain.DLS.get key in
      Stdlib.Domain.DLS.set key (succ ret);
      ret
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

type 'a state = Pending | Resolved of 'a | Failed of exn

type 'a await =
  | Await_all : {
        prm: 'b t
      ; prms: 'a t list
      ; triggered: bool Atomic.t
      ; k: (('a, exn) result list, 'b) State.continuation
    }
      -> 'a await
  | Await_one : {
        prm: 'b t
      ; prms: 'a t list
      ; triggered: bool Atomic.t
      ; and_cancel: bool
      ; k: (('a, exn) result, 'b) State.continuation
    }
      -> 'a await

and 'a cell = Empty | Await of 'a await | Triggered

and 'a t = {
    uid: Promise_uid.t
  ; runner: Domain_uid.t
  ; state: 'a state Atomic.t
  ; parent: pack option
  ; active_children: pack Sequence.t
  ; resources: resource Sequence.t
  ; cell: 'a cell Atomic.t
}

and pack = Pack : 'a t -> pack

module Promise = struct
  module Uid = Promise_uid

  let equal prm0 prm1 = Uid.equal prm0.uid prm1.uid

  let pp ppf { uid; runner; resources; _ } =
    Format.fprintf ppf "[%a:%a](%d)" Domain_uid.pp runner Uid.pp uid
      (Sequence.length resources)

  let transition prm value =
    match (Atomic.get prm.state, value) with
    | (Pending as seen), Ok v ->
        assert (Atomic.compare_and_set prm.state seen (Resolved v))
    | (Pending as seen), Error e ->
        assert (Atomic.compare_and_set prm.state seen (Failed e))
    | _ -> ()

  let cancel prm =
    match Atomic.get prm.state with
    | (Pending | Resolved _ | Failed _) as seen ->
        let cancelled = Failed Cancelled in
        assert (Atomic.compare_and_set prm.state seen cancelled)

  let[@inline never] is_cancelled prm =
    match Atomic.get prm.state with Failed Cancelled -> true | _ -> false

  let[@inline never] is_pending prm =
    match Atomic.get prm.state with Pending -> true | _ -> false

  let[@inline never] is_resolved prm =
    match Atomic.get prm.state with Resolved _ -> true | _ -> false

  let make :
      type a.
         ?g:Random.State.t
      -> resources:resource list
      -> runner:Domain_uid.t
      -> ?parent:a t
      -> unit
      -> _ t =
   fun ?(g = Random.State.make_self_init ()) ~resources:ress ~runner ?parent () ->
    let resources = Sequence.create g in
    List.iter (fun res -> Sequence.push res resources) ress;
    let parent = Option.map (fun parent -> Pack parent) parent in
    {
      uid= Uid.gen ()
    ; runner
    ; state= Atomic.make Pending
    ; parent
    ; active_children= Sequence.create g
    ; resources
    ; cell= Atomic.make Empty
    }

  let pack : type a. a t -> pack = fun prm -> Pack prm

  let to_result prm =
    match Atomic.get prm.state with
    | Pending -> failwith "Promise.to_result"
    | Resolved value -> Ok value
    | Failed exn -> Error exn

  let is_a_children ~parent prm =
    match prm.parent with
    | Some (Pack { uid; _ }) -> Uid.equal uid parent.uid
    | None -> false

  (* This function checks whether all the children of the task associated with
     the promise given as an argument are finished. The function also "cleans
     up" any children that have been cancelled. *)
  let children_terminated prm =
    let to_delete = ref [] in
    let f (Sequence.{ data= Pack prm; _ } as node) =
      if is_cancelled prm then to_delete := node :: !to_delete
    in
    Sequence.iter_node ~f prm.active_children;
    List.iter Sequence.remove !to_delete;
    Sequence.is_empty prm.active_children

  let uid { uid; _ } = uid
end

(** Effects *)

[@@@warning "-30"]

type kind = Concurrent | Parallel of Domain_uid.t
type _ Effect.t += Domain_uid : Domain_uid.t Effect.t
type _ Effect.t += Domain_count : int Effect.t
type _ Effect.t += Spawn : kind * resource list * (unit -> 'a) -> 'a t Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Cancel : 'a t -> unit Effect.t
type _ Effect.t += Domains : Domain_uid.t list Effect.t
type _ Effect.t += Random : Random.State.t Effect.t
type _ Effect.t += Self : (Promise.Uid.t * Domain_uid.t * int) Effect.t
type _ Effect.t += Stats : int Effect.t
type _ Effect.t += Await : 'a t list -> ('a, exn) result list Effect.t
type _ Effect.t += Choose : bool * 'a t list -> ('a, exn) result Effect.t

let pp_kind ppf = function
  | Concurrent -> Fmt.string ppf "Concurrent"
  | Parallel uid -> Fmt.pf ppf "@[<1>(Parallel@ %a)@]" Domain_uid.pp uid

let pp_effect : type a. a Effect.t Fmt.t =
 fun ppf -> function
  | Domain_uid -> Fmt.string ppf "Domain_uid"
  | Domain_count -> Fmt.string ppf "Domain_count"
  | Spawn (k, _ress, _fn) -> Fmt.pf ppf "@[<1>(Spawn@ %a)@]" pp_kind k
  | Yield -> Fmt.string ppf "Yield"
  | Cancel prm -> Fmt.pf ppf "@[<1>(Cancel@ %a)@]" Promise.pp prm
  | Domains -> Fmt.string ppf "Domains"
  | Random -> Fmt.string ppf "Random"
  | Self -> Fmt.string ppf "Self"
  | Stats -> Fmt.string ppf "Stats"
  | Await prms ->
      Fmt.pf ppf "@[<1>](Await@ @[<hov>%a@])@]" Fmt.(Dump.list Promise.pp) prms
  | Choose (_, prms) ->
      Fmt.pf ppf "@[<1>](Choose@ @[<hov>%a@])@]" Fmt.(Dump.list Promise.pp) prms
  | _ -> Fmt.string ppf "#effect"

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
and continue = Continue : Syscall_uid.t * (unit -> unit) -> continue

and domain = {
    g: Random.State.t
  ; tasks: (int * task) Heapq.t
  ; system_tasks: (Syscall_uid.t, system) Hashtbl.t
  ; uid: Domain_uid.t
  ; events: events
  ; quanta: int
  ; tick: int Atomic.t
  ; cancelled_syscalls: Syscall_uid.t Queue.t
  ; pending_events: continue Sequence.t
}

and elt =
  | Create : 'a t * (unit -> 'a) -> elt
  | Delete : 'a t -> elt
  | Signal : {
        prm: 'c t
      ; terminated: Promise.Uid.t list
      ; and_return: 'a
      ; to_cancel: pack list
      ; k: ('a, 'c) State.continuation
    }
      -> elt

and signal_to_dom0 =
  | Signal_to_dom0 : {
        prm: 'c t
      ; terminated: Promise.Uid.t list
      ; and_return: 'a
      ; to_cancel: pack list
      ; k: ('a, 'c) State.continuation
    }
      -> signal_to_dom0

and pool = {
    tasks: elt Sequence.t
  ; mutex: Mutex.t
  ; condition_pending_work: Condition.t
  ; condition_all_idle: Condition.t
  ; domains: domain list
  ; dom0: domain
  ; signals: signal_to_dom0 Queue.t
  ; mutable stop: bool
  ; mutable fail: bool
  ; mutable working_counter: int
  ; mutable domains_counter: int
}

and select = poll:bool -> uid list -> continue list
and events = { select: select; interrupt: unit -> unit }
and handler = { handler: 'u 'v. ('u -> 'v) -> 'u -> 'v } [@@unboxed]
and uid = Syscall_uid.t

let pp_task ppf = function
  | Arrived (prm, _) -> Fmt.pf ppf "@[<1>(Arrived@ %a)@]" Promise.pp prm
  | Suspended (prm, State.Finished _) ->
      Fmt.pf ppf "@[<1>(Finished@ %a)@]" Promise.pp prm
  | Suspended (prm, State.Suspended (_, effect)) ->
      Fmt.pf ppf "@[<1>(Suspended_with@ @[<1>(%a,@ %a)@])@]" Promise.pp prm
        pp_effect effect
  | Suspended (prm, State.Unhandled _) ->
      Fmt.pf ppf "@[<1>(Unhandled@ %a)@]" Promise.pp prm
  | Cancelled prm -> Fmt.pf ppf "@[<1>(Cancelled@ %a)@]" Promise.pp prm
  | Tick -> Fmt.string ppf "Tick"

type _ Effect.t += Suspend : 'a syscall -> 'a Effect.t
type _ Effect.t += Syscall_exists : Syscall_uid.t -> bool Effect.t

let dummy_events = { select= (fun ~poll:_ _ -> []); interrupt= ignore }
let dummy_handler = { handler= apply }

let pp_stats ppf (pool, domain) =
  Fmt.pf ppf
    "[pool:%dw, domain:%dw, domain.tasks:%dw, domain.system_tasks:%dw, \
     domain.pending_events:%dw, cancelled_syscalls:%dw, live_words:%dw]"
    Obj.(reachable_words (repr pool))
    Obj.(reachable_words (repr domain))
    Obj.(reachable_words (repr domain.tasks))
    Obj.(reachable_words (repr domain.system_tasks))
    Obj.(reachable_words (repr domain.pending_events))
    Obj.(reachable_words (repr domain.cancelled_syscalls))
    (Gc.quick_stat ()).Gc.live_words

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

  let compare (tick0, a) (tick1, b) =
    let value = Task.compare a b in
    if value = 0 then Int.compare tick0 tick1 else value

  let dummy = (0, Tick)

  let make ?(quanta = 1) ~g events =
    let uid = Uid.gen () in
    {
      g= Random.State.copy g
    ; uid
    ; tasks= Heapq.create ~compare ~dummy 0x100
    ; system_tasks= Hashtbl.create 0x10
    ; events= events uid
    ; quanta
    ; tick= Atomic.make 0
    ; cancelled_syscalls= Queue.create ()
    ; pending_events= Sequence.create g
    }

  let self () = Effect.perform Domain_uid
  let count () = Effect.perform Domain_count

  (* A domain can "fall" into a waiting state outside Miou via its "select"
     function. As such, the user must propose a way of interrupting domains.
     This is necessary if a task in a domain:
     - has just been cancelled
     - has just been waited for by someone

     In the first case, a new iteration of the domain is required to update its
     tasks (and effectively cancel said task).
     In the second case, the task is connected to a continuation. The latter
     may already have been completed (and the domain is in a waiting state). We
     therefore also need a new iteration to execute the continuation.

     Thus, interruption is essentially seen in 2 cases:
     1) when a new task has been added to the domain (see
        [add_into_pool]/[add_into_dom0])
     2) when a new continuation has been added to a task running on the domain
        (see [await]/[choose])
  *)
  let interrupt pool ~domain:domain' =
    Logs.debug (fun m -> m "interrupts domain [%a]" Domain_uid.pp domain');
    let domain =
      List.find
        (fun domain -> Domain_uid.equal domain' domain.uid)
        (pool.dom0 :: pool.domains)
    in
    domain.events.interrupt ()

  let add_into_pool ~domain pool task =
    Mutex.lock pool.mutex;
    begin
      match task with
      | Create _ | Signal _ -> Sequence.add_r task pool.tasks
      | Delete _ -> Sequence.add_l task pool.tasks
    end;
    Condition.broadcast pool.condition_pending_work;
    Mutex.unlock pool.mutex;
    interrupt pool ~domain

  let add_into_domain domain task =
    let tick = Atomic.fetch_and_add domain.tick 1 in
    Heapq.add domain.tasks (tick, task)

  let add_into_dom0 :
      type a c.
         pool
      -> c t
      -> ?terminated:Promise.Uid.t list
      -> ?to_cancel:pack list
      -> and_return:a
      -> (a, c) State.continuation
      -> unit =
   fun pool prm ?(terminated = []) ?(to_cancel = []) ~and_return k ->
    Queue.enqueue pool.signals
      (Signal_to_dom0 { prm; terminated; to_cancel; and_return; k });
    pool.dom0.events.interrupt ()

  let suspend_system_call domain (Syscall (uid, fn) : _ syscall) prm k =
    match Hashtbl.find domain.system_tasks uid with
    | System_call_suspended _ ->
        Fmt.failwith "%a has already been suspended" Promise.pp prm
    | exception Not_found ->
        Hashtbl.replace domain.system_tasks uid
          (System_call_suspended (uid, fn, prm, k))

  let clean_system_tasks domain prm =
    let set = Hashtbl.create 0x10 in
    Hashtbl.filter_map_inplace
      (fun uid (System_call_suspended (_, _, prm', _) as suspended) ->
        if Promise.equal prm prm' then begin
          Logs.debug (fun m ->
              m "[%a] clean the syscall [%a] (system-tasks)" Domain_uid.pp
                domain.uid Syscall_uid.pp uid);
          Hashtbl.add set uid ();
          Queue.enqueue domain.cancelled_syscalls uid;
          None
        end
        else Some suspended)
      domain.system_tasks;
    let to_delete = ref [] in
    let f (Sequence.{ data= Continue (uid', _); _ } as node) =
      if Hashtbl.mem set uid' then begin
        Logs.debug (fun m ->
            m "[%a] clean the syscall [%a] (system-events)" Domain_uid.pp
              domain.uid Syscall_uid.pp uid');
        to_delete := node :: !to_delete
      end
    in
    Sequence.iter_node ~f domain.pending_events;
    List.iter Sequence.remove !to_delete

  let clean_system_task_if_suspended domain = function
    | State.Suspended (_, Suspend syscall) ->
        let (Syscall (uid, _)) = syscall in
        Queue.enqueue domain.cancelled_syscalls uid
    | _ -> ()

  (* This function is properly the cancellation of a task (whether it works in
     the current domain or not). If it belongs to another domain, we [interrupt]
     said domain and add a cancellation task to our [pool]. Otherwise, we just
     need to add the cancellation task to our heap and reschedule.

     Note that cancellation tasks take priority. *)
  let terminate pool domain (Pack prm) =
    Logs.debug (fun m ->
        m "[%a] signal to cancel %a" Domain_uid.pp domain.uid Promise.pp prm);
    if not (Domain_uid.equal prm.runner domain.uid) then
      add_into_pool ~domain:prm.runner pool (Delete prm)
    else begin
      clean_system_tasks domain prm;
      add_into_domain domain (Cancelled prm)
    end

  type 'a Effect.t +=
    | Spin : {
          terminated: Promise.Uid.t list
        ; to_cancel: pack list
        ; and_return: 'a
      }
        -> 'a Effect.t

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

  let error_already_owned prm (Resource { uid; _ }) =
    Invalid_argument
      (Fmt.str "Miou.own: the resource [%a] was already owned by %a"
         Domain_uid.pp uid Promise.pp prm)

  let error_impossible_to_transfer prm =
    Invalid_argument
      (Fmt.str "Miou.Ownership.transfer: impossible to transfer from %a"
         Promise.pp prm)

  let own_it prm (Resource { uid; _ }) =
    let own_it = ref false in
    let f (Resource { uid= uid'; _ }) =
      own_it := !own_it || Resource_uid.equal uid uid'
    in
    Sequence.iter ~f prm.resources;
    !own_it

  let ownership :
      type a b. _ t -> (a State.Op.t -> b State.t) -> a ownership -> b State.t =
   fun prm k eff ->
    let open State in
    match eff with
    | Check (Resource { uid; _ } as res) ->
        Logs.debug (fun m ->
            m "check if %a owns [%a] (%b)" Promise.pp prm Resource_uid.pp uid
              (own_it prm res));
        if own_it prm res = false then k (Op.fail Not_owner)
        else k (Op.return ())
    | Own (Resource { uid; _ } as res) ->
        Logs.debug (fun m ->
            m "%a owns [%a]" Promise.pp prm Resource_uid.pp uid);
        if own_it prm res = false then begin
          Sequence.push res prm.resources;
          k (Op.return res)
        end
        else k (Op.fail (error_already_owned prm res))
    | Disown (Resource { uid; _ } as res) ->
        Logs.debug (fun m ->
            m "%a disown [%a] (own it? %b)" Promise.pp prm Resource_uid.pp uid
              (own_it prm res));
        if own_it prm res then begin
          let to_delete = ref Option.none in
          let f (Sequence.{ data= Resource { uid= uid'; _ }; _ } as node) =
            if uid = uid' then to_delete := Option.some node
          in
          Sequence.iter_node ~f prm.resources;
          let[@warning "-8"] (Option.Some node) = !to_delete in
          Sequence.remove node;
          k (Op.return ())
        end
        else k (Op.fail Not_owner)
    | Transfer (Resource { uid; _ } as res) ->
        if own_it prm res then begin
          match prm.parent with
          | None -> k (Op.fail (error_impossible_to_transfer prm))
          | Some (Pack prm') ->
              let to_transmit = ref Option.none in
              let f (Sequence.{ data= Resource { uid= uid'; _ }; _ } as node) =
                if uid = uid' then to_transmit := Option.some node
              in
              Sequence.iter_node ~f prm.resources;
              let[@warning "-8"] (Option.Some ({ Sequence.data; _ } as node)) =
                !to_transmit
              in
              Sequence.push data prm'.resources;
              Sequence.remove node;
              k (Op.return ())
        end
        else k (Op.fail Not_owner)

  let clean_children ~children current =
    let to_delete = ref [] in
    let f (Sequence.{ data= Pack prm; _ } as node) =
      if List.exists (Promise.Uid.equal prm.uid) children then
        to_delete := node :: !to_delete
    in
    Sequence.iter_node ~f current.active_children;
    List.iter Sequence.remove !to_delete

  let perform pool domain (Pack current) =
    let perform :
        type a b. (a State.Op.t -> b State.t) -> a Effect.t -> b State.t =
     fun k eff ->
      let open State in
      Logs.debug (fun m ->
          m "[%a] %a perform %a" Domain_uid.pp domain.uid Promise.pp current
            pp_effect eff);
      match eff with
      | Domain_uid -> k (Op.return domain.uid)
      | Domain_count -> k (Op.return (List.length pool.domains))
      | Domains ->
          let uids = List.map (fun { uid; _ } -> uid) pool.domains in
          k (Op.return uids)
      | Random -> k (Op.return domain.g)
      | Self ->
          let uid = current.uid
          and runner = current.runner
          and resources = Sequence.length current.resources in
          k (Op.return (uid, runner, resources))
      | Stats ->
          let w = Obj.(reachable_words (repr current)) in
          k (Op.return w)
      | Spawn (Concurrent, resources, fn) ->
          let runner = domain.uid in
          let prm =
            Promise.make ~g:domain.g ~resources ~runner ~parent:current ()
          in
          Logs.debug (fun m ->
              m "[%a] create a new concurrent task %a" Domain_uid.pp domain.uid
                Promise.pp prm);
          Sequence.add_l (Promise.pack prm) current.active_children;
          add_into_domain domain (Arrived (prm, fn));
          k (Op.return prm)
      | Spawn (Parallel runner, resources, fn) ->
          assert (Domain_uid.equal runner domain.uid = false);
          let prm =
            Promise.make ~g:domain.g ~resources ~runner ~parent:current ()
          in
          Logs.debug (fun m ->
              m "[%a] create a new parallel task %a" Domain_uid.pp domain.uid
                Promise.pp prm);
          Sequence.add_l (Promise.pack prm) current.active_children;
          add_into_pool ~domain:runner pool (Create (prm, fn));
          k (Op.return prm)
      | Yield ->
          (* TODO(dinosaure): we probably need to specify more strictly the
             behavior of [yield]. Indeed, depending on the [quanta], the result
             is not the same. It is also probably related to our note about
             [Arrived] (see [handle]) where we should start to consume effects
             via [State.run]. *)
          k Op.yield
      | Spin { terminated; to_cancel= []; and_return } ->
          clean_children ~children:terminated current;
          k (Op.return and_return)
      | Spin { terminated; to_cancel; and_return } ->
          let terminated, to_cancel =
            List.fold_left
              (fun (terminated, prms) (Pack prm) ->
                if Promise.is_cancelled prm then (prm.uid :: terminated, prms)
                else (terminated, Pack prm :: prms))
              (terminated, []) to_cancel
          in
          k (Op.continue (Spin { terminated; to_cancel; and_return }))
      | Cancel prm when Promise.is_cancelled prm ->
          Logs.debug (fun m ->
              m "[%a] %a cancels %a (cancelled)" Domain_uid.pp domain.uid
                Promise.pp current Promise.pp prm);
          if Promise.is_a_children ~parent:current prm then begin
            clean_children ~children:[ prm.uid ] current;
            k (Op.return ())
          end
          else k (Op.fail Not_a_child)
      | Cancel prm ->
          Logs.debug (fun m ->
              m "[%a] %a cancels %a" Domain_uid.pp domain.uid Promise.pp current
                Promise.pp prm);
          if Promise.is_a_children ~parent:current prm then (
            terminate pool domain (Pack prm);
            k
              (Op.continue
                 (Spin
                    { terminated= []; to_cancel= [ Pack prm ]; and_return= () })))
          else k (Op.fail Not_a_child)
      | Ownership action -> ownership current k action
      | Await _ -> k Op.interrupt
      | Choose _ -> k Op.interrupt
      | Suspend _ -> k Op.interrupt
      | effect -> k (Op.perform effect)
    in
    { State.perform }

  let resource_leak prm =
    let leak = ref false in
    let f (Resource { uid; finaliser; value }) =
      Logs.debug (fun m ->
          m "resource [%a] leaked on %a" Resource_uid.pp uid Promise.pp prm);
      let () =
        try finaliser value
        with exn ->
          Logs.err (fun m ->
              m "finaliser of [%a] raised an exception: %S" Resource_uid.pp uid
                (Printexc.to_string exn))
      in
      leak := true
    in
    Sequence.iter ~f prm.resources;
    !leak

  let trigger pool domain current and_return =
    assert (Domain_uid.equal current.runner domain.uid);
    Logs.debug (fun m ->
        m "[%a] %a finished, check if it trigger something." Domain_uid.pp
          domain.uid Promise.pp current);
    (* NOTE(dinosaure): if we can not do the transition from [Empty] to
       [Triggered], this means that the parent task has added a continuation to
       the current task (via [await]). So we need to manage this continuation to
       pass it the result of our task via an inter-domain transfer (the parent
       task may be running on a domain other than the current one).

       If several tasks are part of the queue, the continuation is managed with
       the value [triggered]. If this value has already been modified, it means
       that a task has already passed on its value. Our task should therefore be
       cancelled ([await_first]), waiting for the other tasks to be finished
       ([await_all]) or unchanged ([await_one]). *)
    if Atomic.compare_and_set current.cell Empty Triggered = false then
      match Atomic.get current.cell with
      | Empty ->
          assert false
          (* NOTE(dinosaure): we were not able to do the transition, this means
             that [current.cell] must not be [Empty]. *)
      | Triggered ->
          ()
          (* NOTE(dinosaure): no continuation has been connected to the task,
             you can just do nothing. *)
      | Await (Await_all { prm; prms; triggered; k }) ->
          let all_terminated =
            List.for_all (Fun.negate Promise.is_pending) prms
          in
          Logs.debug (fun m ->
              m "[%a] %a => %a (all terminated: %b)" Domain_uid.pp domain.uid
                Promise.pp current Promise.pp prm all_terminated);
          if all_terminated && Atomic.compare_and_set triggered false true then begin
            let terminated = List.map (fun ({ uid; _ } : _ t) -> uid) prms in
            let and_return = List.map Promise.to_result prms in
            if Domain_uid.equal prm.runner domain.uid then
              add_into_domain domain
                (Suspended
                   ( prm
                   , State.suspended_with k
                       (Spin { terminated; to_cancel= []; and_return }) ))
            else if prm.runner = 0 then
              add_into_dom0 pool prm ~terminated ~and_return k
            else
              add_into_pool ~domain:prm.runner pool
                (Signal { prm; terminated; and_return; to_cancel= []; k })
          end
      | Await (Await_one { prm; prms; triggered; and_cancel; k }) ->
          Logs.debug (fun m ->
              m "[%a] %a => %a" Domain_uid.pp domain.uid Promise.pp current
                Promise.pp prm);
          let an_other_is_resolved = List.exists Promise.is_resolved prms in
          let current_failed = not (Promise.is_resolved current) in
          let ignore = an_other_is_resolved && current_failed in
          if (not ignore) && Atomic.compare_and_set triggered false true then begin
            Logs.debug (fun m ->
                m "[%a] %a is chosen as the resolved promise" Domain_uid.pp
                  domain.uid Promise.pp current);
            let prms = List.map Promise.pack prms in
            if and_cancel then List.iter (terminate pool domain) prms;
            let to_cancel = if and_cancel then prms else [] in
            let terminated = [ current.uid ] in
            if Domain_uid.equal prm.runner domain.uid then
              add_into_domain domain
                (Suspended
                   ( prm
                   , State.suspended_with k
                       (Spin { terminated; to_cancel; and_return }) ))
            else if prm.runner = 0 then
              add_into_dom0 pool prm ~terminated ~to_cancel ~and_return k
            else
              add_into_pool ~domain:prm.runner pool
                (Signal { prm; terminated; to_cancel; and_return; k })
          end

  let handle pool domain prm = function
    | State.Finished (Error exn) when Promise.children_terminated prm ->
        Logs.debug (fun m ->
            m "%a failed with %S" Promise.pp prm (Printexc.to_string exn));
        let f (Resource { finaliser; value; uid; _ }) =
          Logs.debug (fun m ->
              m "[%a] finalise the resource [%a]" Domain_uid.pp domain.uid
                Resource_uid.pp uid);
          try finaliser value
          with exn ->
            Logs.err (fun m ->
                m "[%a] finaliser of [%a] raised an exception: %S" Domain_uid.pp
                  domain.uid Resource_uid.pp uid (Printexc.to_string exn))
        in
        Sequence.iter ~f prm.resources;
        Sequence.drop prm.resources;
        (* XXX(dinosaure): [trigger] can re-add:
           [Suspended (prm, State.Supended (_, Spin _)_)] into our domain. We
           must clear resources to avoid a double-[Unix.close] (for instance). *)
        assert (Domain_uid.equal domain.uid prm.runner);
        Promise.transition prm (Error exn);
        trigger pool domain prm (Error exn)
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
           the [once] (and the [Cancelled prm when Promise.is_cancelled prm]
           case) function which consumes tasks). *)
        Logs.debug (fun m ->
            m "[%a] %a finished with an error, cancel %d children" Domain_uid.pp
              domain.uid Promise.pp prm
              (Sequence.length prm.active_children));
        Sequence.iter ~f:(terminate pool domain) prm.active_children;
        add_into_domain domain (Suspended (prm, State.pure (Error exn)))
    | State.Finished (Ok value) ->
        Logs.debug (fun m -> m "%a resolved" Promise.pp prm);
        if Promise.children_terminated prm = false then raise Still_has_children;
        if resource_leak prm then raise Resource_leak;
        assert (Domain_uid.equal domain.uid prm.runner);
        Promise.transition prm (Ok value);
        trigger pool domain prm (Ok value)
    | State.Suspended _ as state ->
        add_into_domain domain (Suspended (prm, state))
    | State.Unhandled _ as state ->
        Logs.debug (fun m ->
            m "%a suspended due to unhandled effect" Promise.pp prm);
        add_into_domain domain (Suspended (prm, state))

  let transfer_system_task domain (Continue (uid, fn0) as continue : continue) =
    match Hashtbl.find_opt domain.system_tasks uid with
    | None ->
        Logs.debug (fun m ->
            m "[%a] [%a] not found, keep it for the next iteration"
              Domain_uid.pp domain.uid Syscall_uid.pp uid);
        Sequence.add_l continue domain.pending_events
    | Some (System_call_suspended (_, fn1, prm, k)) -> (
        Hashtbl.remove domain.system_tasks uid;
        Logs.debug (fun m ->
            m "[%a] consumes the system task [%a]" Domain_uid.pp domain.uid
              Syscall_uid.pp uid);
        let state =
          try
            fn0 ();
            State.continue_with k (fn1 ())
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            State.discontinue_with ~backtrace:bt k exn
        in
        match state with
        | State.Finished value ->
            assert (Domain_uid.equal domain.uid prm.runner);
            Promise.transition prm value;
            (* NOTE(dinosaure): the transition should be done by [handle] from
               the computed [state]. So we should execute [handle] here.
               However, [handle] can generate continuations via the [trigger]
               function that should "not yet" be executed.

               At this point in the process, we've probably already executed a
               [handle] and therefore consumed a quanta. System event management
               must be delayed to the next iteration. However, system events can
               result in contradictory cases, such as a choice between 2 system
               events, one of which involves a cancellation:

               {[
                 let run stop fd =
                   let accept () = Miou.call_cc @@ fun () ->
                     Miou_unix.accept fd in
                   let stop = Miou.call_cc @@ fun () ->
                     Miou_unix.Cond.wait stop;
                     raise Timeout in
                   Miou.await_first [ accept; stop ]
               ]}

               These choices are determined by the state of the promises. So, we
               still need to change the state of the promises in relation to
               what these system events imply: if a system event implies the
               resolution of a promise, we need to make the transition
               **before** the next iteration.

               So, at the next iteration, the state of the promises will be up
               to date with what has been implied by the system events. At the
               next iteration, we'll be able to [handle] these tasks
               correctly. *)
            add_into_domain domain (Suspended (prm, state))
        | _ -> add_into_domain domain (Suspended (prm, state)))

  let await :
      type a b.
         pool
      -> domain
      -> a t
      -> ((b, exn) result list, a) State.continuation
      -> b t list
      -> unit =
   fun pool domain current k prms ->
    (* NOTE(dinosaure): [current] is a promise associated to a task which run
       into the given [domain]. [prm] is a promise given by a [Miou.await]
       (performed by the [current]'s task) and the latter's task _can_ run into
       another domain.

       The goal of this function is to _save_ the continuation [k] into
       [prm] so that at the end of its task, we trigger/run/continue the
       [current]'s task. However, one rule exists: a domain cannot transmit
       a task to [dom0]. We must filter the case where the [current]'s task
       actually run into [dom0] and the [prm]'s task run into different domain.
       In such case, (eg. [| 0, false ->]), we just check if the [prm]'s task
       was finished and reschedule this observation if it's not. In the other
       case, we save the continuation [k] into [prm]. When the [prm]'s task is
       completed, it will schedule/transmit the continuation [k] to the correct
       domain of the [current]'s task (see [trigger]).

       If the [current]'s task and the [prm]'s task run into [dom0], it's safe
       to save the continuation. Scheduling the continuation will be local, so
       there will be no transmission from a domain to [dom0]. *)
    assert (Domain_uid.equal domain.uid current.runner);
    Logs.debug (fun m ->
        m "[%a] promises %a are finished: %b" Domain_uid.pp domain.uid
          Fmt.(Dump.list Promise.pp)
          prms
          (List.for_all (Fun.negate Promise.is_pending) prms));
    if List.for_all (Promise.is_a_children ~parent:current) prms = false then
      let state = State.discontinue_with k Not_a_child in
      add_into_domain domain (Suspended (current, state))
    else begin
      (* NOTE(dinosaure): 2 behaviors can be implemented here:
         1) we can try to attach the continuation to all given promises and
            check if we updated, at least, one of these promises. If it's not
            the case, this means that all the tasks have been terminated, so you
            can "continue" straight away.
         2) or we can check that all promises are terminated and, if it's the
            case, we do the continuation.

         Actually, we implement the first behavior. It's not sure that the
         second case is "faster" - it depends on how users use Miou.
      *)
      let triggered = Atomic.make false in
      let attach prm =
        Logs.debug (fun m ->
            m "[%a] add a new waiter (%a) for %a" Domain_uid.pp domain.uid
              Promise.pp current Promise.pp prm);
        let await = Await_all { prm= current; prms; triggered; k } in
        let attached =
          Promise.is_cancelled prm = false
          && Atomic.compare_and_set prm.cell Empty (Await await)
        in
        if attached && Domain_uid.equal prm.runner domain.uid = false then
          interrupt pool ~domain:prm.runner;
        attached
      in
      let states = List.map attach prms in
      let all_terminated = List.for_all Fun.(negate id) states in
      if all_terminated then begin
        (* XXX(dinosaure): [really_await], all [prms] are terminated. *)
        assert (List.for_all (Fun.negate Promise.is_pending) prms);
        let results = List.map Promise.to_result prms in
        assert (Domain_uid.equal current.runner domain.uid);
        let children = List.map (fun ({ uid; _ } : _ t) -> uid) prms in
        clean_children ~children current;
        let state =
          State.suspended_with k
            (Spin { terminated= []; to_cancel= []; and_return= results })
        in
        add_into_domain domain (Suspended (current, state))
      end
    end

  let iter_with_exclusion ~f prms =
    let rec go prms' = function
      | [] -> ()
      | prm :: prms ->
          f (List.rev_append prms' prms) prm;
          go (prm :: prms') prms
    in
    go [] prms

  let really_choose pool domain current k ~and_cancel prms =
    let to_choose =
      match List.filter Promise.is_resolved prms with
      | [] -> List.filter (Fun.negate Promise.is_pending) prms
      | resolved -> resolved
    in
    let len = List.length to_choose in
    let prm = List.nth to_choose (Random.State.int domain.g len) in
    let and_return = Promise.to_result prm in
    let others =
      List.filter
        (fun (prm' : _ t) -> Promise_uid.equal prm'.uid prm.uid = false)
        prms
    in
    let others = List.map Promise.pack others in
    if and_cancel then List.iter (terminate pool domain) others;
    let to_cancel = if and_cancel then others else [] in
    let terminated = [ prm.uid ] in
    assert (Domain_uid.equal current.runner domain.uid);
    add_into_domain domain
      (Suspended
         ( current
         , State.suspended_with k (Spin { terminated; to_cancel; and_return })
         ))

  let choose pool domain current k ~and_cancel prms =
    if List.for_all (Promise.is_a_children ~parent:current) prms = false then
      let state = State.discontinue_with k Not_a_child in
      add_into_domain domain (Suspended (current, state))
    else if List.exists (Fun.negate Promise.is_pending) prms then
      really_choose pool domain current k ~and_cancel prms
    else
      let triggered = Atomic.make false in
      let attach prms prm =
        let await =
          Await_one { prm= current; k; prms; triggered; and_cancel }
        in
        Atomic.set prm.cell (Await await);
        if Domain_uid.equal prm.runner domain.uid = false then
          interrupt pool ~domain:prm.runner
      in
      iter_with_exclusion ~f:attach prms

  let get_promise_into_domain ~uid domain =
    let tasks = ref [] in
    let f (_, task) =
      match task with
      | Cancelled _ | Tick -> ()
      | Arrived (prm, _) ->
          if Promise_uid.equal prm.uid uid then tasks := task :: !tasks
      | Suspended (prm, _) ->
          if Promise_uid.equal prm.uid uid then tasks := task :: !tasks
      (* Morally, this function should also aggregate cancellations. However,
         the only time it's used is when there's a cancellation. In this case,
         the aim is not to search for all occurrences of our promise in the
         heap, but to find out whether our task has just arrived or has been
         suspended. *)
    in
    Heapq.iter f domain.tasks;
    match !tasks with
    | [ (Arrived _ as elt) ] | [ (Suspended _ as elt) ] -> Some elt
    | [] -> None
    | tasks ->
        Logs.err (fun m ->
            m "[%a] we have a task (promise [%a]) with several states: %a"
              Domain_uid.pp domain.uid Promise_uid.pp uid
              Fmt.(Dump.list pp_task)
              tasks);
        assert false
  (* It is normally impossible to have a task that is both arrived and in a
     suspended state. It's either one or the other. If we fall into this
     situation, our assertions are wrong. *)

  let once pool domain task =
    match task with
    | Tick -> Domain.cpu_relax ()
    | Arrived (prm, _fn) when Promise.is_cancelled prm ->
        handle pool domain prm (State.pure (Error Cancelled))
    | Cancelled prm when Promise.is_cancelled prm ->
        (* XXX(dinosaure): if a promise is already cancelled, we don't need to
           retry the cancellation. The main issue if we miss this branch is the
           apparition of several [Suspended (Finished (Error Cancelled)] with
           the initial suspension of the task which produce the cancellation: so
           we have several states for one task which is wrong. So we must do
           nothing when we try to cancel a cancelled promise. *)
        handle pool domain prm (State.pure (Error Cancelled))
    | Suspended (prm, state) when Promise.is_cancelled prm ->
        handle pool domain prm state
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
        let state = State.make fn () in
        handle pool domain prm state
    | Suspended (current, State.Suspended (k, Suspend syscall)) ->
        let (Syscall (uid, _)) = syscall in
        Logs.debug (fun m ->
            m "[%a] new system suspension point for [%a] into %a" Domain_uid.pp
              domain.uid Syscall_uid.pp uid Promise.pp current);
        suspend_system_call domain syscall current k
    | Suspended (current, State.Suspended (k, Await prms)) ->
        Logs.debug (fun m ->
            m "[%a] await %a" Domain_uid.pp domain.uid
              Fmt.(Dump.list Promise.pp)
              prms);
        await pool domain current k prms
    | Suspended (current, State.Suspended (k, Choose (and_cancel, prms))) ->
        choose pool domain current k ~and_cancel prms
    | Suspended (prm, state) ->
        let perform = perform pool domain (Promise.pack prm) in
        let state = State.run ~quanta:domain.quanta ~perform state in
        handle pool domain prm state
    | Cancelled prm -> (
        Logs.debug (fun m ->
            m "[%a] %a cancelled" Domain_uid.pp domain.uid Promise.pp prm);
        match get_promise_into_domain ~uid:prm.uid domain with
        | Some (Suspended (prm', state)) ->
            assert (Promise.Uid.equal prm'.uid prm.uid);
            assert (Domain_uid.equal domain.uid prm'.runner);
            clean_system_task_if_suspended domain state;
            clean_system_tasks domain prm';
            Promise.cancel prm';
            handle pool domain prm' (State.fail ~exn:Cancelled state)
        | Some (Arrived (prm', _)) ->
            assert (Promise.Uid.equal prm'.uid prm.uid);
            assert (Domain_uid.equal domain.uid prm'.runner);
            clean_system_tasks domain prm';
            Promise.cancel prm';
            handle pool domain prm (State.pure (Error Cancelled))
        | Some (Cancelled _ | Tick) ->
            () (* Impossible case, but let's do nothing *)
        | None ->
            assert (Domain_uid.equal domain.uid prm.runner);
            clean_system_tasks domain prm;
            Promise.cancel prm;
            handle pool domain prm (State.pure (Error Cancelled)))

  let transmit_pending_events domain =
    let syscalls = Sequence.to_list domain.pending_events in
    Sequence.drop domain.pending_events;
    List.iter (transfer_system_task domain) syscalls

  let unblock_awaits_with_system_tasks domain =
    let open Effect.Deep in
    let retc = Fun.id in
    let exnc = reraise in
    let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'a) option =
      function
      | Syscall_exists uid ->
          let yes = Hashtbl.mem domain.system_tasks uid in
          Some (fun k -> continue k yes)
      | _ -> None
    in
    let handler = { retc; exnc; effc } in
    let poll =
      Heapq.length domain.tasks = 0 && Sequence.length domain.pending_events = 0
    in
    let syscalls = Queue.(to_list (transfer domain.cancelled_syscalls)) in
    let syscalls = match_with (domain.events.select ~poll) syscalls handler in
    Logs.debug (fun m ->
        m "[%a] got %d syscalls" Domain_uid.pp domain.uid (List.length syscalls));
    List.iter (transfer_system_task domain) syscalls

  let system_tasks_suspended domain = Hashtbl.length domain.system_tasks > 0

  let one_task_for ~domain (pool : pool) =
    let exception Yes in
    let f = function
      | Create (prm, _) when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace Yes
      | Delete prm when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace Yes
      | Signal { prm; _ } when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace Yes
      | _ -> ()
    in
    try
      Sequence.iter ~f pool.tasks;
      false
    with Yes -> true

  let run pool domain () =
    transmit_pending_events domain;
    match Heapq.pop_minimum domain.tasks with
    | exception Heapq.Empty ->
        if system_tasks_suspended domain then
          unblock_awaits_with_system_tasks domain
    | _tick, elt ->
        once pool domain elt;
        if system_tasks_suspended domain then
          unblock_awaits_with_system_tasks domain
end

module Pool = struct
  let nothing_to_do (pool : pool) (domain : domain) =
    Heapq.is_empty domain.tasks
    && Sequence.is_empty domain.pending_events
    && Domain.system_tasks_suspended domain = false
    && Domain.one_task_for ~domain pool = false

  let rec transfer_all_tasks (pool : pool) (domain : domain) =
    let exception Task of elt Sequence.node in
    let f ({ Sequence.data; _ } as node) =
      match data with
      | Delete prm when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace (Task node)
      | Create (prm, _) when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace (Task node)
      | Signal { prm; _ } when Domain_uid.equal prm.runner domain.uid ->
          raise_notrace (Task node)
      | _ -> ()
    in
    try Sequence.iter_node ~f pool.tasks
    with Task ({ Sequence.data; _ } as node) ->
      Sequence.remove node;
      begin
        match data with
        | Delete prm ->
            Logs.debug (fun m ->
                m "[%a] delete a task %a" Domain_uid.pp domain.uid Promise.pp
                  prm);
            Domain.add_into_domain domain (Cancelled prm)
        | Create (prm, fn) ->
            Logs.debug (fun m ->
                m "[%a] create a task %a" Domain_uid.pp domain.uid Promise.pp
                  prm);
            Domain.add_into_domain domain (Arrived (prm, fn))
        | Signal { prm; terminated; and_return; to_cancel; k } ->
            Logs.debug (fun m ->
                m "[%a] continue a task %a" Domain_uid.pp domain.uid Promise.pp
                  prm);
            let spin = Domain.Spin { terminated; to_cancel; and_return } in
            let k = State.suspended_with k spin in
            Domain.add_into_domain domain (Suspended (prm, k))
      end;
      transfer_all_tasks pool domain

  (* The "worker" has a very simple operation: it waits for a signal from the
     other domains and sees if it has something to do. If so, it transfers
     ([transfer_all_tasks]) the tasks locally and runs [Domain.run]. Finally, it
     goes back to sleep until the next signal. Domains can communicate with
     [dom0] (the launcher) by signaling that all domains are dormant ([idle]).
     Finally, [dom0] can communicate with the domains asking to stop. *)
  let worker pool domain () =
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
        Logs.debug (fun m -> m "%a" pp_stats (pool, domain));
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
        (* NOTE(dinosaure): this exception appears only by
           [Domain.run pool domain ()]. At this stage, [pool.mutex] is unlocked!
           We must re-lock it. *)
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

  let make ?(quanta = 1) ~dom0 ?(handler = dummy_handler)
      ?(domains = number_of_domains ()) events =
    let domains =
      List.init domains @@ fun _ -> Domain.make ~quanta ~g:dom0.g events
    in
    let pool =
      {
        tasks= Sequence.create dom0.g
      ; mutex= Mutex.create ()
      ; condition_pending_work= Condition.create ()
      ; condition_all_idle= Condition.create ()
      ; stop= false
      ; fail= false
      ; working_counter= 0
      ; domains_counter= List.length domains
      ; domains
      ; dom0
      ; signals= Queue.create ()
      }
    in
    (* NOTE(dinosaure): we apply the user's handler here but we should use it
       when we call [Domain.run] as [dom0] does. *)
    let spawn domain =
      Stdlib.Domain.spawn (handler.handler (worker pool domain))
    in
    (pool, List.map spawn domains)
end

(* Functions and modules which are exposed to the interface. *)

module Ownership = struct
  type t = resource
  and uid = Resource_uid.t

  let own ~finally:finaliser value =
    let res = Resource.make ~finaliser value in
    Effect.perform (Ownership (Own res))

  let disown res = Effect.perform (Ownership (Disown res))
  let transfer res = Effect.perform (Ownership (Transfer res))
  let check res = Effect.perform (Ownership (Check res))
  let uid (Resource { uid; _ }) = uid
  let pp = Resource_uid.pp
end

let domains () = Effect.perform Domains
let random () = Effect.perform Random
let self () = Effect.perform Self
let stats () = Effect.perform Stats

type 'a orphans = 'a t Sequence.t

let orphans () = Sequence.create (random ())

let care t =
  let exception Found in
  let ready = ref Option.none in
  let f (Sequence.{ data= { state; _ }; _ } as node) =
    match Atomic.get state with
    | Pending -> ()
    | _ ->
        ready := Option.some node;
        raise_notrace Found
  in
  if Sequence.length t > 0 then begin
    try
      Sequence.iter_node ~f t;
      Option.(some none)
    with Found ->
      let[@warning "-8"] (Option.Some ({ Sequence.data; _ } as node)) =
        !ready
      in
      Sequence.remove node;
      Option.(some (some data))
  end
  else None

let call ?orphans ?(give = []) fn =
  let domains = domains () in
  if domains = [] then raise No_domain_available;
  let cur = Domain.self () in
  let uid =
    let g = random () in
    match List.filter (Fun.negate (Domain_uid.equal cur)) domains with
    | [] -> raise No_domain_available
    | lst -> List.nth lst (Random.State.int g (List.length lst))
  in
  let prm = Effect.perform (Spawn (Parallel uid, give, fn)) in
  Option.iter (Sequence.push prm) orphans;
  prm

let call_cc ?orphans ?(give = []) fn =
  let prm = Effect.perform (Spawn (Concurrent, give, fn)) in
  Option.iter (Sequence.push prm) orphans;
  prm

let make fn =
  let uid = Syscall_uid.gen () in
  Syscall (uid, fn)

let suspend syscall = Effect.perform (Suspend syscall)
let await prm = Effect.perform (Await [ prm ]) |> List.hd
let yield () = Effect.perform Yield
let cancel prm = Effect.perform (Cancel prm)
let await_all prms = Effect.perform (Await prms)
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

let await_one = function
  | [] -> invalid_arg "Miou.await_one"
  | prms -> Effect.perform (Choose (false, prms))

let await_first = function
  | [] -> invalid_arg "Miou.await_first"
  | prms -> Effect.perform (Choose (true, prms))

let await_exn prm =
  match await prm with Ok value -> value | Error exn -> reraise exn

let both prm0 prm1 =
  let res0 = await prm0 and res1 = await prm1 in
  (res0, res1)

let continue_with (Syscall (uid, _) : _ syscall) fn : continue =
  Continue (uid, fn)

let quanta =
  match Sys.getenv_opt "MIOU_QUANTA" with
  | Some str -> ( try int_of_string str with _ -> 1)
  | None -> 1

let domains =
  match Sys.getenv_opt "MIOU_DOMAINS" with
  | Some str -> ( try int_of_string str with _ -> Pool.number_of_domains ())
  | None -> Pool.number_of_domains ()

let transfer_continuations_into_dom0 pool dom0 =
  let continuations = Queue.(to_list (transfer pool.signals)) in
  let transfer (Signal_to_dom0 { prm; terminated; and_return; to_cancel; k }) =
    Logs.debug (fun m ->
        m "[%a] received a signal for %a" Domain_uid.pp dom0.uid Promise.pp prm);
    let spin = Domain.Spin { terminated; to_cancel; and_return } in
    let state = State.suspended_with k spin in
    Domain.add_into_domain dom0 (Suspended (prm, state))
  in
  List.iter transfer continuations

let run ?(quanta = quanta) ?(events = Fun.const dummy_events)
    ?(g = Random.State.make_self_init ()) ?(domains = domains)
    ?(handler = dummy_handler) fn =
  Domain.Uid.reset ();
  let dom0 = Domain.make ~quanta ~g events in
  let prm0 = Promise.make ~g:dom0.g ~resources:[] ~runner:dom0.uid () in
  Domain.add_into_domain dom0 (Arrived (prm0, fn));
  let pool, domains = Pool.make ~quanta ~dom0 ~domains ~handler events in
  let result =
    try
      while Promise.is_pending prm0 && not pool.fail do
        transfer_continuations_into_dom0 pool dom0;
        handler.handler (Domain.run pool dom0) ();
        Logs.debug (fun m -> m "%a" pp_stats (pool, dom0));
        Logs.debug (fun m -> m "[prm0:%dw]" Obj.(reachable_words (repr prm0)))
      done;
      if not pool.fail then Promise.to_result prm0
      else Error (Failure "A domain failed")
        (* XXX(dinosaure): if [pool.fail = true], a domain re-raised the
           exception it got during the process. Event if we return
           [Failure "A domain failed"], we should get the initial exception via
           [Domain.join]. *)
    with exn -> Error exn
  in
  Pool.kill pool;
  List.iter Stdlib.Domain.join domains;
  match result with Ok value -> value | Error exn -> reraise exn
