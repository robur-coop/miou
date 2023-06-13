open Miou

type file_descr = { fd: Unix.file_descr; non_blocking: bool }

type unix_scheduler = {
    rd: (Unix.file_descr, rd) Hashtbl.t
  ; wr: (Unix.file_descr, wr) Hashtbl.t
  ; sleepers: (Id.t, float * unit Prm.t) Hashtbl.t
}

and rd =
  [ `Read of unit Prm.t * bytes * int * int
  | `Accept of unit Prm.t * bool option ]

and wr =
  [ `Write of unit Prm.t * string * int * int | `In_progress of unit Prm.t ]

let dom =
  let make () =
    {
      rd= Hashtbl.create 0x100
    ; wr= Hashtbl.create 0x100
    ; sleepers= Hashtbl.create 0x100
    }
  in
  let dom = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get dom

let minimum sleepers =
  let fold uid (until, prm) = function
    | Some (_, until', _) when until < until' -> Some (uid, until, prm)
    | Some _ as acc -> acc
    | None -> Some (uid, until, prm)
  in
  Hashtbl.fold fold sleepers None

let sleeper () =
  let dom = dom () in
  match minimum dom.sleepers with
  | Some (uid, _, prm) ->
      let k () = Hashtbl.remove dom.sleepers uid in
      Some [ Miou.syscall prm k ]
  | None -> None

let events () =
  let dom = dom () in
  let ts =
    match minimum dom.sleepers with Some (_, until, _) -> until | None -> 0.
  in
  let t0 = Unix.gettimeofday () in
  match Unix.select [] [] [] ts with
  | exception Unix.(Unix_error (EINTR, _, _)) -> None
  | [], [], _ ->
      let t1 = Unix.gettimeofday () in
      Hashtbl.filter_map_inplace
        (fun _ (until, prm) ->
          let until' = Float.max 0. (until -. (t1 -. t0)) in
          Some (until', prm))
        dom.sleepers;
      sleeper ()
  | _ -> assert false

let run ?g fn = Miou.run ~events ?g fn

let sleep until =
  let dom = dom () in
  let prm = Prm.make ~return:(Fun.const ()) in
  Hashtbl.add dom.sleepers (Prm.uid prm) (until, prm);
  Prm.await prm
