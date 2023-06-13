open Miou

let sleepers = Hashtbl.create 0x100

let sleep until =
  let promise = Prm.make ~return:(Fun.const ()) in
  Hashtbl.add sleepers (Prm.uid promise) (promise, until);
  Prm.await promise

let events () =
  Format.eprintf "sleepers: %d\n%!" (Hashtbl.length sleepers);
  let min =
    Hashtbl.fold
      (fun uid (prm, until) -> function
        | Some (_uid', _prm', until') when until < until' ->
            Format.eprintf ">>> %f < %f\n%!" until until';
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
        (fun _ (prm, until') -> Some (prm, Float.min 0. (until' -. until)))
        sleepers;
      Format.eprintf ">>> sleep %f\n%!" until;
      Unix.sleepf until;
      Some [ Miou.syscall prm (Fun.const ()) ]

let program () =
  Miou.run ~events @@ fun () ->
  let a = Prm.call_cc (fun () -> sleep 1.) in
  let b = Prm.call_cc (fun () -> sleep 2.) in
  Prm.await_all_ign [ a; b ]

let () =
  let t0 = Unix.gettimeofday () in
  program ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)
