(* NOTE(dinosaure): this code is an **experimentation** with a /box/. The
   implementation does not work! But it works in some situations that we show
   up at the end of this code. It ensures a certain behavior of [miou]
   regardless [Miou.call], [Miou.call_cc] and [Miou.suspend]. *)

type box = Box : 'a option Atomic.t * 'a Miou.syscall -> box
type t = (Miou.uid, box) Hashtbl.t

let dom =
  let make () : t = Hashtbl.create 0x100 in
  let dom = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get dom

module Box : sig
  type 'a t
  type 'a u

  val make : unit -> 'a t * 'a u
  val push : 'a -> 'a u -> bool
  val take : 'a t -> 'a
end = struct
  type 'a t = 'a Miou.syscall
  type 'a u = 'a option Atomic.t

  let make () =
    let tbl = dom () in
    let value = Atomic.make None in
    let prm =
      Miou.make @@ fun () ->
      match Atomic.get value with
      | Some value -> value
      | None -> invalid_arg "box is empty"
    in
    Hashtbl.add tbl (Miou.uid prm) (Box (value, prm));
    (prm, value)

  let push value u = Atomic.compare_and_set u None (Some value)
  let take t = Miou.suspend t
end

let select ~poll:_ _ =
  let tbl = dom () in
  let fold uid (Box (value, prm)) acc =
    match Atomic.get value with
    | Some _ -> Miou.task prm (fun () -> Hashtbl.remove tbl uid) :: acc
    | None -> acc
  in
  Hashtbl.fold fold tbl []

let events _domain = { Miou.select; interrupt= ignore }

let prgm0 () =
  let t, u = Box.make () in
  let p0 = Miou.call @@ fun () -> assert (Box.push () u) in
  Miou.await_exn p0; Box.take t

let prgm1 () =
  let t, u = Box.make () in
  let p0 = Miou.call @@ fun () -> ignore (Box.push () u) in
  Box.take t; Miou.await_exn p0

let prgm2 () =
  let t, u = Box.make () in
  let p0 = Miou.call_cc @@ fun () -> ignore (Box.push () u) in
  Miou.await_exn p0; Box.take t

let () = Miou.run ~events prgm0
let () = Miou.run ~events prgm1
let () = Miou.run ~events prgm2
