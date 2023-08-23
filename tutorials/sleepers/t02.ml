open Miou

let sleepers =
  let make () = Hashtbl.create 0x100 in
  let key = Stdlib.Domain.DLS.new_key make in
  fun () -> Stdlib.Domain.DLS.get key

let sleep until =
  let return () = () in
  let promise = Miou.make return in
  let sleepers = sleepers () in
  Hashtbl.add sleepers (Miou.uid promise) (promise, until);
  Miou.suspend promise

let select () =
  let sleepers = sleepers () in
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
      let until = Float.min until 0.100 in
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
  let t0 = Clock.now () in
  program ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 3.)
