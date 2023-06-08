open Miou

module Timer = struct
  type t = { uid: Id.t; until: float }

  let compare a b = Stdlib.compare a.until b.until
  let dummy = { uid= Id.null; until= max_float }
end

module Sleep = Binary_heap.Make (Timer)

type unix_scheduler = { sleepers: (Id.t, unit Sys.t) Hashtbl.t * Sleep.t }

let dom =
  let make () =
    { sleepers= (Hashtbl.create 256, Sleep.create ~dummy:Timer.dummy 256) }
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
      let syscall = Hashtbl.find tbl uid in
      let k () = Sleep.remove sleepers in
      Some (Syscall (syscall, k))
  | exception Binary_heap.Empty -> None

let events () =
  let dom = dom () in
  let timeout = next_sleeper dom.sleepers in
  match Unix.select [] [] [] timeout with
  | exception Unix.(Unix_error (EINTR, _, _)) -> None
  | _ -> resolve_sleeper ()

let run ?g fn = Miou.run ~events ?g fn

let sleep until =
  let dom = dom () in
  let tbl, sleepers = dom.sleepers in
  let var = Sys.make () in
  Sleep.add sleepers { uid= Sys.uid var; until };
  Hashtbl.add tbl (Sys.uid var) var;
  Sys.syscall var
