open Miou

let sleepers = Hashtbl.create 0x100

let sleep until =
  let promise = Prm.make ~return:(Fun.const ()) in
  Hashtbl.add sleepers (Prm.uid promise) (promise, until);
  Prm.await promise

let events () =
  let min =
    Hashtbl.fold
      (fun uid (prm, until) -> function
        | Some (_uid', _prm', until') when until < until' ->
            Some (uid, prm, until)
        | Some _ as acc -> acc
        | None -> Some (uid, prm, until))
      sleepers None
  in
  match min with
  | None -> None
  | Some (uid, prm, until) ->
      Hashtbl.remove sleepers uid;
      Hashtbl.filter_map_inplace
        (fun _ (prm, until') -> Some (prm, Float.max 0. (until' -. until)))
        sleepers;
      Unix.sleepf until;
      Some [ Miou.syscall prm (Fun.const ()) ]

let prgm () =
  Miou.run ~events @@ fun () ->
  let a = Prm.call_cc (fun () -> sleep 0.2) in
  let b = Prm.call_cc (fun () -> sleep 0.4) in
  ignore (Prm.await a);
  ignore (Prm.await b)

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 0.6)
