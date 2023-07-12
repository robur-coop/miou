let sleepers =
  let make () = Hashtbl.create 0x100 in
  let key = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get key

let sleep until =
  let promise = Miou.make (Fun.const ()) in
  let sleepers = sleepers () in
  Hashtbl.add sleepers (Miou.uid promise) (promise, until);
  match Miou.suspend promise with Ok () -> () | Error exn -> raise exn

let rec consume_interrupt ic =
  if Unix.read ic (Bytes.create 1) 0 1 = 0 then consume_interrupt ic

let update sleepers n =
  Hashtbl.filter_map_inplace
    (fun _ (prm, until) ->
      let until' = Float.max 0. (until -. n) in
      Some (prm, until'))
    sleepers

let select interrupt () =
  let sleepers = sleepers () in
  Hashtbl.filter_map_inplace
    (fun _ (prm, until) ->
      if Miou.is_pending prm then Some (prm, until) else None)
    sleepers;
  let min =
    Hashtbl.fold
      (fun uid (prm, until) -> function
        | Some (_uid', _prm', until') when until < until' ->
            Some (uid, prm, until)
        | Some _ as acc -> acc
        | None -> Some (uid, prm, until))
      sleepers None
  in
  let ts =
    Option.map (fun (_, _, until) -> until) min |> function
    | Some ts -> ts
    | None -> 0.
  in
  let t0 = Unix.gettimeofday () in
  match Unix.select [ interrupt ] [] [] ts with
  | [], _, _ -> (
      let t1 = Unix.gettimeofday () in
      update sleepers (t1 -. t0);
      match min with
      | Some (_, prm, _) -> [ Miou.task prm (Fun.const ()) ]
      | None -> [])
  | _ ->
      let t1 = Unix.gettimeofday () in
      update sleepers (t1 -. t0);
      consume_interrupt interrupt;
      []

let events _ =
  let ic, oc = Unix.pipe ~cloexec:true () in
  let rec interrupt () =
    if Unix.write oc (Bytes.make 1 '\000') 0 1 = 0 then interrupt ()
  in
  { Miou.select= select ic; interrupt }

let program0 () =
  Miou.run ~events @@ fun () ->
  let a = Miou.call_cc (fun () -> sleep 1.) in
  let b = Miou.call_cc (fun () -> sleep 2.) in
  Miou.await_all [ a; b ]
  |> List.iter (function Ok () -> () | Error exn -> raise exn)

let program1 () =
  Miou.run ~events @@ fun () ->
  let a = Miou.call (fun () -> sleep 1.) in
  let b = Miou.call (fun () -> sleep 2.) in
  Miou.await_all [ a; b ]
  |> List.iter (function Ok () -> () | Error exn -> raise exn)

let program2 () =
  Miou.run ~events @@ fun () ->
  let a = Miou.call (fun () -> sleep 10.) in
  sleep 1.;
  Miou.cancel a;
  match Miou.await a with Error Miou.Cancelled -> () | _ -> failwith "test"

let () =
  Format.eprintf "[0]: %!";
  let t0 = Unix.gettimeofday () in
  program0 ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.);
  Format.eprintf "ok\n%!"

let () =
  Format.eprintf "[1]: %!";
  let t0 = Unix.gettimeofday () in
  program1 ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.);
  Format.eprintf "ok\n%!"

let () =
  Format.eprintf "[2]: %!";
  let t0 = Unix.gettimeofday () in
  program2 ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 10.);
  Format.eprintf "ok\n%!"
