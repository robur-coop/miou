(* NOTE(dinosaure): This example is our tutorial about sleepers. *)

let sleepers = Hashtbl.create 0x100

let sleep until =
  let syscall = Miou.make (Fun.const ()) in
  Hashtbl.add sleepers (Miou.uid syscall) (syscall, until);
  Miou.suspend syscall

let select () =
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
  | None -> []
  | Some (uid, prm, until) ->
      Hashtbl.remove sleepers uid;
      Hashtbl.filter_map_inplace
        (fun _ (prm, until') -> Some (prm, Float.max 0. (until' -. until)))
        sleepers;
      Unix.sleepf until;
      [ Miou.task prm (Fun.const ()) ]

let events _ = { Miou.select; interrupt= ignore }

let prgm () =
  Miou.run ~events @@ fun () ->
  let a = Miou.call_cc (fun () -> sleep 1.) in
  let b = Miou.call_cc (fun () -> sleep 2.) in
  Miou.await_all [ a; b ] |> ignore

let () =
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 3.)
