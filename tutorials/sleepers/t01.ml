open Miou

let sleepers = Hashtbl.create 0x100

let sleep until =
  let return () = () in
  let promise = Miou.make return in
  Hashtbl.add sleepers (Miou.uid promise) (promise, until);
  match Miou.suspend promise with Ok () -> () | Error exn -> raise exn

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

let events _ = { select; interrupt= ignore }

let program () =
  Miou.run ~events @@ fun () ->
  let a = Miou.call_cc (fun () -> sleep 1.) in
  let b = Miou.call_cc (fun () -> sleep 2.) in
  Miou.await_all [ a; b ]
  |> List.iter @@ function Ok () -> () | Error exn -> raise exn

let () =
  let t0 = Unix.gettimeofday () in
  program ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)
