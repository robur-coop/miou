let get, set =
  let make () = Hashtbl.create 0x100 in
  let key = Stdlib.Domain.DLS.new_key make in
  let get () = Stdlib.Domain.DLS.get key in
  let set = Stdlib.Domain.DLS.set key in
  (get, set)

let sleep until =
  let sleepers = get () in
  let syscall = Miou.syscall () in
  Hashtbl.add sleepers (Miou.uid syscall) (syscall, until);
  set sleepers;
  Miou.suspend syscall

let consume_interrupt ic =
  let buf = Bytes.create 0x1000 in
  let _ = Unix.read ic buf 0 (Bytes.length buf) in
  ()

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
        cs := Miou.signal syscall :: !cs;
        None)
      else Some (syscall, until))
    sleepers;
  !cs

let select interrupt ~block cancelled =
  let sleepers = get () in
  Hashtbl.filter_map_inplace
    (fun _ (syscall, until) ->
      if List.exists (( = ) (Miou.uid syscall)) cancelled then None
      else Some (syscall, until))
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
    | None -> if block then -1.0 else 0.100
  in
  let t0 = Unix.gettimeofday () in
  match Unix.select [ interrupt ] [] [] ts with
  | [], _, _ ->
      let t1 = Unix.gettimeofday () in
      update sleepers (t1 -. t0);
      set sleepers;
      minimums sleepers
  | _ ->
      let t1 = Unix.gettimeofday () in
      update sleepers (t1 -. t0);
      consume_interrupt interrupt;
      set sleepers;
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
