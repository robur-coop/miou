let sleepers =
  let make () = Hashtbl.create 0x100 in
  let key = Stdlib.Domain.DLS.new_key make in
  fun () -> Stdlib.Domain.DLS.get key

let sleep until =
  let syscall = Miou.make (Fun.const ()) in
  let sleepers = sleepers () in
  Hashtbl.add sleepers (Miou.uid syscall) (syscall, until);
  Miou.suspend syscall

let rec consume_interrupt ic =
  if Unix.read ic (Bytes.create 1) 0 1 = 0 then consume_interrupt ic

let update sleepers n =
  Hashtbl.filter_map_inplace
    (fun _ (syscall, until) ->
      let until' = Float.max 0. (until -. n) in
      Some (syscall, until'))
    sleepers

let minimums sleepers =
  let cs = ref [] in
  Hashtbl.filter_map_inplace
    (fun _ (syscall, until) ->
      if until <= 0. then (
        cs := Miou.task syscall (Fun.const ()) :: !cs;
        None)
      else Some (syscall, until))
    sleepers;
  !cs

let select interrupt ~poll:_ _ =
  let sleepers = sleepers () in
  Hashtbl.filter_map_inplace
    (fun _ (syscall, until) ->
      if Miou.is_pending syscall then Some (syscall, until) else None)
    sleepers;
  let min =
    Hashtbl.fold
      (fun uid (syscall, until) -> function
        | Some (_uid', _syscall', until') when until < until' ->
            Some (uid, syscall, until)
        | Some _ as acc -> acc
        | None -> Some (uid, syscall, until))
      sleepers None
  in
  let ts =
    Option.map (fun (_, _, until) -> until) min |> function
    | Some ts -> Float.min ts 0.100
    | None -> 0.
  in
  let t0 = Unix.gettimeofday () in
  match Unix.select [ interrupt ] [] [] ts with
  | [], _, _ ->
      let t1 = Unix.gettimeofday () in
      update sleepers (t1 -. t0);
      minimums sleepers
  | _ ->
      let t1 = Unix.gettimeofday () in
      update sleepers (t1 -. t0);
      consume_interrupt interrupt;
      minimums sleepers

let events _ =
  let ic, oc = Unix.pipe ~cloexec:true () in
  let rec interrupt () =
    if Unix.write oc (Bytes.make 1 '\000') 0 1 = 0 then interrupt ()
  in
  { Miou.select= select ic; interrupt }

let () =
  let t0 = Unix.gettimeofday () in
  let () =
    Miou.run ~events @@ fun () ->
    let a = Miou.call_cc (fun () -> sleep 1.) in
    let b = Miou.call_cc (fun () -> sleep 2.) in
    Miou.await_all [ a; b ]
    |> List.iter (function Ok () -> () | Error exn -> raise exn)
  in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)

let () =
  let t0 = Unix.gettimeofday () in
  let () =
    Miou.run ~events @@ fun () ->
    Miou.parallel sleep [ 1.; 2. ]
    |> List.iter (function Ok () -> () | Error exn -> raise exn)
  in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)

let () =
  let t0 = Unix.gettimeofday () in
  let () =
    Miou.run ~events @@ fun () ->
    let a = Miou.call (fun () -> sleep 10.) in
    sleep 1.;
    Miou.cancel a;
    match Miou.await a with Error Miou.Cancelled -> () | _ -> failwith "test"
  in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 10.)
