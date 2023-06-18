module Tq = Tq

module Did = struct
  type t = int

  let equal = Int.equal
  let compare = Int.compare

  let gen, reset =
    let v = Atomic.make 0 in
    let gen () = Atomic.fetch_and_add v 1 and reset () = Atomic.set v 0 in
    (gen, reset)

  let parallel = gen
  let pp = Format.pp_print_int

  type _ Effect.t += Domain_id : t Effect.t

  let concurrent () = Effect.perform Domain_id
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

let always_none = Fun.const None
let or_raise = function Ok value -> value | Error exn -> raise exn

module Cond = struct
  type t = Mutex.t * Condition.t

  let signal (m, c) = Mutex.lock m; Condition.signal c; Mutex.unlock m

  let wait ~predicate (m, c) =
    let res = ref false in
    Mutex.lock m;
    if
      res := predicate ();
      !res
    then Condition.wait c m;
    Mutex.unlock m;
    !res

  let make () = (Mutex.create (), Condition.create ())
end

module Pool = struct
  type t = { mutable running: int; maximum: int; wait: Cond.t }

  let make ?(maximum = Domain.recommended_domain_count () - 1) () =
    { running= 0; maximum; wait= Cond.make () }

  let rec wait t =
    let predicate () = t.running >= t.maximum in
    if Cond.wait ~predicate t.wait then wait t

  let unsafe_add t =
    Mutex.lock (fst t.wait);
    t.running <- t.running + 1;
    Mutex.unlock (fst t.wait)

  let unsafe_del t =
    Mutex.lock (fst t.wait);
    t.running <- t.running - 1;
    Condition.signal (snd t.wait);
    Mutex.unlock (fst t.wait)
end

module Ty = struct
  type 'a t = Domain of Pool.t option | Task | Block of (unit -> 'a)
end

module Prm = struct
  exception Cancelled

  type 'a state =
    | Pending
    | Resolved of 'a
    | Failed of exn
    | Consumed of ('a, exn) result

  type 'a t = {
      domain: Did.t
    ; uid: Id.t
    ; ty: 'a Ty.t
    ; state: 'a state Atomic.t
    ; children: w Tq.t
  }

  and w = Prm : 'a t -> w

  let uid { uid; _ } = uid

  let pp ppf { domain; uid; _ } =
    Format.fprintf ppf "[%a:%a]" Did.pp domain Id.pp uid

  type _ Effect.t += Spawn : 'a t * (unit -> 'a) -> 'a t Effect.t

  let call_cc fn =
    let domain = Did.concurrent () in
    let uid = Id.gen () in
    let prm =
      {
        domain
      ; uid
      ; ty= Task
      ; state= Atomic.make Pending
      ; children= Tq.make ()
      }
    in
    Effect.perform (Spawn (prm, fn))

  let call ?pool fn =
    let domain = Did.parallel () in
    let uid = Id.gen () in
    let prm =
      {
        domain
      ; uid
      ; ty= Domain pool
      ; state= Atomic.make Pending
      ; children= Tq.make ()
      }
    in
    Option.iter Pool.wait pool;
    Option.iter Pool.unsafe_add pool;
    Effect.perform (Spawn (prm, fn))

  let make ~return =
    let domain = Did.concurrent () in
    let uid = Id.gen () in
    let prm =
      {
        domain
      ; uid
      ; ty= Block return
      ; state= Atomic.make Pending
      ; children= Tq.make ()
      }
    in
    Effect.perform (Spawn (prm, return))

  type _ Effect.t += Await : 'a t -> ('a, exn) result Effect.t

  let await prm = Effect.perform (Await prm)
  let await_exn prm = or_raise (await prm)

  type _ Effect.t += Await_first : 'a t list -> ('a, exn) result Effect.t

  let await_first prms =
    if prms = [] then invalid_arg "Prm.await_first";
    Effect.perform (Await_first prms)

  let await_first_exn prms = or_raise (await_first prms)

  type _ Effect.t += Await_all : 'a t list -> ('a, exn) result list Effect.t

  let await_all prms =
    if prms = [] then invalid_arg "Prm.await_all";
    Effect.perform (Await_all prms)

  let await_all_ign prms =
    let lst = await_all prms in
    (* TODO(dinosaure): filter our exception (Not_a_child | Still_has_children) *)
    match List.partition Result.is_error lst with
    | Error exn :: _, _ -> raise exn
    | _ -> ()

  let is_pending prm =
    match Atomic.get prm.state with Pending -> true | _ -> false

  let rec is_consumed : type a. a t -> bool =
   fun prm ->
    match Atomic.get prm.state with
    | Consumed _ ->
        let res = ref true in
        Tq.iter
          ~f:(fun (Prm prm) -> res := is_consumed prm && !res)
          prm.children;
        !res
    | _ -> false

  let to_resolved prm value =
    ignore (Atomic.compare_and_set prm.state Pending (Resolved value))

  let rec failed_with : type a. a t -> exn -> unit =
   fun prm exn ->
    Tq.iter ~f:(fun (Prm p) -> cancelled p) prm.children;
    if not (Atomic.compare_and_set prm.state Pending (Consumed (Error exn)))
    then
      match Atomic.get prm.state with
      | Resolved v as seen ->
          ignore (Atomic.compare_and_set prm.state seen (Consumed (Ok v)))
      | Failed exn as seen ->
          ignore (Atomic.compare_and_set prm.state seen (Consumed (Error exn)))
      | _ -> ()

  and cancelled : type a. a t -> unit =
   fun prm ->
    Tq.iter ~f:(fun (Prm p) -> cancelled p) prm.children;
    ignore (Atomic.compare_and_set prm.state Pending (Failed Cancelled))

  let cancel prm = failed_with prm Cancelled

  let to_failed prm exn =
    Tq.iter ~f:(fun (Prm p) -> cancelled p) prm.children;
    ignore (Atomic.compare_and_set prm.state Pending (Failed exn))

  let rec abnormal : type a. a t -> exn -> unit =
   fun prm exn ->
    Tq.iter ~f:(fun (Prm p) -> abnormal p exn) prm.children;
    if not (Atomic.compare_and_set prm.state Pending (Consumed (Error exn)))
    then
      match Atomic.get prm.state with
      | Resolved v as seen ->
          ignore (Atomic.compare_and_set prm.state seen (Consumed (Ok v)))
      | Failed exn as seen ->
          ignore (Atomic.compare_and_set prm.state seen (Consumed (Error exn)))
      | _ -> ()

  let to_result_exn prm =
    match Atomic.get prm.state with
    | Resolved v -> Ok v
    | Failed exn -> Error exn
    | Consumed r -> r
    | Pending -> invalid_arg "Prm.to_result_exn"
end

type process =
  | Task : 'a Prm.t * (unit -> 'a) -> process
  | Domain : domain * 'a Prm.t * unit Domain.t -> process
  | Unblock : 'a unblock -> process

and 'a continuation = (('a, exn) result, unit) Effect.Deep.continuation
and block = Block : 'a Prm.t * (unit -> 'a) * 'a continuation -> block

and 'a unblock = {
    prm: 'a Prm.t
  ; fn: unit -> unit
  ; return: unit -> 'a
  ; k: 'a continuation
}

and domain = {
    g: Random.State.t
  ; glist: process L.t
  ; blist: block L.t
  ; uid: Did.t
  ; parent: Cond.t option
  ; busy: Cond.t
  ; events: unit -> syscall list option
}

and go = { go: 'a. domain -> 'a Prm.t -> (unit -> 'a) -> unit } [@@unboxed]
and syscall = Syscall : 'a Prm.t * (unit -> unit) -> syscall

exception Not_a_child
exception Still_has_children
exception Unresolvable

type _ Effect.t += Yield : unit Effect.t

let yield () = Effect.perform Yield

let transfer domain tasks =
  let transfer syscall =
    let (Syscall (prm, fn)) = syscall in
    let res = ref None in
    let f node =
      let (Block (prm', _, _)) = L.data node in
      if Id.equal prm.Prm.uid prm'.Prm.uid then res := Some node
    in
    L.iter_on ~f domain.blist;
    match !res with
    | Some node ->
        let (Block (prm, return, k)) = L.data node in
        let unblock = { prm; fn; return; k } in
        L.remove node;
        L.push (Unblock unblock) domain.glist
    | None -> ()
  in
  List.iter transfer tasks

let step domain go =
  match L.take domain.glist with
  | Task (prm, fn) -> (
      match Atomic.get prm.Prm.state with
      | Prm.Pending -> go.go domain prm fn
      | _ -> ())
  | Domain (_, prm, _) as process when Prm.is_pending prm ->
      L.push process domain.glist
  | Domain (_, _, value) -> ignore (Domain.join value)
  | Unblock { prm; fn; return; k } -> (
      go.go domain prm (fun () -> fn (); return ());
      match Atomic.get prm.Prm.state with
      | Resolved value as seen ->
          let res = Ok value in
          ignore (Atomic.compare_and_set prm.Prm.state seen (Consumed res));
          Effect.Deep.continue k res
      | Failed exn as seen ->
          let res = Error exn in
          ignore (Atomic.compare_and_set prm.Prm.state seen (Consumed res));
          Effect.Deep.continue k res
      | Pending -> raise Unresolvable
      | Consumed _ -> ())

let rec run domain go =
  match L.is_empty domain.glist with
  | false -> step domain go; run domain go
  | true -> (
      match domain.events () with
      | Some [] -> Domain.cpu_relax (); run domain go
      | None -> ()
      | Some tasks -> transfer domain tasks; run domain go)

let is_a_child ~parent prm =
  let res = ref false in
  let f (Prm.Prm child) = res := !res || child.uid = prm.Prm.uid in
  Tq.iter ~f parent.Prm.children;
  !res

let all_processes_are_pending_domains domain =
  let res = ref (not (L.is_empty domain.glist)) in
  let f = function
    | Domain (_, prm, _) -> res := !res && Prm.is_pending prm
    | _ -> res := false
  in
  L.iter ~f domain.glist; !res

let await_domains_or_tasks domain go =
  let predicate () = all_processes_are_pending_domains domain in
  if not (Cond.wait ~predicate domain.busy) then step domain go

let consumed ?k prm =
  match Atomic.get prm.Prm.state with
  | Prm.Pending -> false
  | Prm.Resolved value as seen ->
      let res = Ok value in
      ignore (Atomic.compare_and_set prm.Prm.state seen (Consumed res));
      Option.iter (fun k -> Effect.Deep.continue k res) k;
      true
  | Prm.Failed exn as seen ->
      let res = Error exn in
      ignore (Atomic.compare_and_set prm.Prm.state seen (Consumed res));
      Option.iter (fun k -> Effect.Deep.continue k res) k;
      true
  | Prm.Consumed res ->
      Option.iter (fun k -> Effect.Deep.continue k res) k;
      true

let do_await domain ~parent go prm k =
  if not (is_a_child ~parent prm) then Effect.Deep.discontinue k Not_a_child
  else if not (consumed ~k prm) then
    match (Atomic.get prm.Prm.state, prm.Prm.ty) with
    | Prm.Pending, Ty.Task ->
        run domain go;
        ignore (consumed ~k prm)
    | Prm.Pending, Ty.Domain _ ->
        let rec until_is_resolved () =
          await_domains_or_tasks domain go;
          match Atomic.get prm.Prm.state with
          | Resolved value as seen ->
              let res = Ok value in
              ignore (Atomic.compare_and_set prm.Prm.state seen (Consumed res));
              Effect.Deep.continue k res
          | Failed exn as seen ->
              let res = Error exn in
              ignore (Atomic.compare_and_set prm.Prm.state seen (Consumed res));
              Effect.Deep.discontinue k exn
          | Consumed res -> Effect.Deep.continue k res
          | Pending -> until_is_resolved ()
        in
        until_is_resolved ()
    | Prm.Pending, Ty.Block return ->
        L.push (Block (prm, return, k)) domain.blist;
        run domain go
    | Prm.(Resolved _ | Failed _ | Consumed _), _ ->
        (* NOTE(dinosaure): this case is already handled by [consumed]. *) ()

let do_await_first domain go prms k =
  let rec until prms =
    let resolved, unresolved = List.partition (consumed ?k:None) prms in
    match resolved with
    | [] ->
        await_domains_or_tasks domain go;
        until unresolved
    | _ ->
        let len = List.length resolved in
        let idx = Random.State.int domain.g len in
        let prm = List.nth resolved idx in
        List.iter (fun prm -> Prm.abnormal prm Prm.Cancelled) unresolved;
        Effect.Deep.continue k (Prm.to_result_exn prm)
  in
  until prms

let do_await_all domain go prms k =
  let rec until unresolved =
    let _, unresolved = List.partition (consumed ?k:None) unresolved in
    match unresolved with
    | [] ->
        let lst = List.map Prm.to_result_exn prms in
        Effect.Deep.continue k lst
    | _ ->
        await_domains_or_tasks domain go;
        until unresolved
  in
  until prms

let do_domain_id domain k = Effect.Deep.continue k domain.uid
let do_yield domain go k = run domain go; Effect.Deep.continue k ()

let do_spawn domain ~parent go prm fn k =
  Tq.enqueue parent.Prm.children (Prm.Prm prm);
  match prm.Prm.ty with
  | Ty.Task ->
      L.push (Task (prm, fn)) domain.glist;
      Effect.Deep.continue k prm
  | Ty.Domain pool ->
      let g = Random.State.copy domain.g in
      let domain' =
        {
          g
        ; glist= L.make g
        ; blist= L.make g
        ; uid= prm.Prm.domain
        ; events= domain.events
        ; busy= Cond.make ()
        ; parent= Some domain.busy
        }
      in
      let value =
        Domain.spawn @@ fun () ->
        let finally () =
          Option.iter Pool.unsafe_del pool;
          Cond.signal domain.busy
        in
        let fn () = Fun.protect ~finally fn in
        go.go domain' prm fn
      in
      L.push (Domain (domain', prm, value)) domain.glist;
      Effect.Deep.continue k prm
  | Ty.Block _ -> Effect.Deep.continue k prm

let collect_pending_children prm =
  let rec go pending =
    match Tq.dequeue prm.Prm.children with
    | Prm.Prm prm when Prm.is_consumed prm -> go pending
    | Prm.Prm prm -> go (Prm.Prm prm :: pending)
    | exception Tq.Empty -> pending
  in
  go []

let syscall prm fn = Syscall (prm, fn)

let run ?(g = Random.State.make_self_init ()) ?(events = always_none) fn =
  let rec go : type a. domain -> a Prm.t -> (unit -> a) -> unit =
   fun domain prm fn ->
    let retc value =
      match collect_pending_children prm with
      | [] ->
          Prm.to_resolved prm value;
          Option.iter Cond.signal domain.parent
      | _ ->
          Prm.to_failed prm Still_has_children;
          Option.iter Cond.signal domain.parent
    in
    let exnc exn =
      Prm.abnormal prm exn;
      Option.iter Cond.signal domain.parent
    in
    let effc :
        type a.
        a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option =
     fun eff ->
      match (Atomic.get prm.Prm.state, eff) with
      | Prm.(Failed exn | Consumed (Error exn)), _ ->
          Some (fun k -> Effect.Deep.discontinue k exn)
      | _, Did.Domain_id -> Some (do_domain_id domain)
      | _, Yield -> Some (do_yield domain { go })
      | _, Prm.Await prm' -> Some (do_await domain ~parent:prm { go } prm')
      | _, Prm.Await_first prms -> Some (do_await_first domain { go } prms)
      | _, Prm.Await_all prms -> Some (do_await_all domain { go } prms)
      | _, Prm.Spawn (prm', fn) ->
          Some (do_spawn domain ~parent:prm { go } prm' fn)
      | _ -> None
    in
    Effect.Deep.match_with fn () { Effect.Deep.retc; exnc; effc }
  in
  Did.reset ();
  let dom0 =
    {
      g
    ; uid= Did.parallel ()
    ; glist= L.make g
    ; blist= L.make g
    ; events
    ; busy= Cond.make ()
    ; parent= None
    }
  in
  let prm =
    {
      Prm.uid= Id.gen ()
    ; domain= dom0.uid
    ; state= Atomic.make Prm.Pending
    ; ty= Ty.Domain None
    ; children= Tq.make ()
    }
  in
  go dom0 prm fn; Prm.to_result_exn prm

let run ?g ?events fn = or_raise (run ?g ?events fn)
