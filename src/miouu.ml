open Miou

module Timer = struct
  type t = { uid: Id.t; until: float }

  let compare a b = Stdlib.compare a.until b.until
  let dummy = { uid= Id.null; until= max_float }
end

module Sleep = Binary_heap.Make (Timer)

type file_descr = { fd: Unix.file_descr; non_blocking: bool }

type unix_scheduler = {
    rd: (Unix.file_descr, rd) Hashtbl.t
  ; wr: (Unix.file_descr, wr) Hashtbl.t
  ; sleepers: (Id.t, unit Prm.t) Hashtbl.t * Sleep.t
}

and rd =
  [ `Read of unit Prm.t * bytes * int * int
  | `Accept of unit Prm.t * bool option ]

and wr =
  [ `Write of unit Prm.t * string * int * int | `In_progress of unit Prm.t ]

let dom =
  let make () =
    {
      rd= Hashtbl.create 256
    ; wr= Hashtbl.create 256
    ; sleepers= (Hashtbl.create 256, Sleep.create ~dummy:Timer.dummy 256)
    }
  in
  let dom = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get dom

let next_sleeper (_, sleepers) =
  match Sleep.minimum sleepers with
  | { until; _ } -> until
  | exception Binary_heap.Empty -> 0.

let resolve_sleeper () =
  let dom = dom () in
  let tbl, sleepers = dom.sleepers in
  match Sleep.minimum sleepers with
  | { uid; _ } ->
      let prm = Hashtbl.find tbl uid in
      (* TODO(dinosaure): rebalance sleeper. *)
      let k () = Sleep.remove sleepers in
      Some [ Miou.syscall prm k ]
  | exception Binary_heap.Empty -> None

let events () =
  let dom = dom () in
  let timeout = next_sleeper dom.sleepers in
  match Unix.select [] [] [] timeout with
  | exception Unix.(Unix_error (EINTR, _, _)) ->
      Format.eprintf ">>> EINTR\n%!";
      None
  | [], [], _ -> resolve_sleeper ()
  | _ -> assert false

let run ?g fn = Miou.run ~events ?g fn

let sleep until =
  let dom = dom () in
  let tbl, sleepers = dom.sleepers in
  let prm = Prm.make ~return:(Fun.const ()) in
  Sleep.add sleepers { uid= Prm.uid prm; until };
  Hashtbl.add tbl (Prm.uid prm) prm;
  Prm.await prm
