open Miou

let get, set =
  let make () = Hashtbl.create 0x100 in
  let key = Stdlib.Domain.DLS.new_key make in
  let get () = Stdlib.Domain.DLS.get key in
  let set value = Stdlib.Domain.DLS.set key value in
  (get, set)

let sleep until =
  let return () = () in
  let syscall = Miou.make return in
  let sleepers = get () in
  Hashtbl.add sleepers (Miou.uid syscall) (syscall, until);
  set sleepers;
  Miou.suspend syscall

let select ~poll:_ _ =
  let sleepers = get () in
  let min =
    Hashtbl.fold
      (fun uid (syscall, until) -> function
        | Some (_uid', _syscall', until') when until < until' ->
            Some (uid, syscall, until)
        | Some _ as acc -> acc
        | None -> Some (uid, syscall, until))
      sleepers None
  in
  match min with
  | None -> []
  | Some (_, _, until) ->
      let until = Float.min until 0.100 in
      Unix.sleepf until;
      Hashtbl.filter_map_inplace
        (fun _ (syscall, until') ->
          Some (syscall, Float.max 0. (until' -. until)))
        sleepers;
      let continuations =
        Hashtbl.fold
          (fun uid (syscall, until) acc ->
            if until <= 0. then
              Miou.continue_with syscall (fun () -> Hashtbl.remove sleepers uid)
              :: acc
            else acc)
          sleepers []
      in
      set sleepers; continuations

let events _ = { select; interrupt= ignore }

let () =
  let t0 = Unix.gettimeofday () in
  let () =
    Miou.run ~events @@ fun () ->
    let a = Miou.call_cc (fun () -> sleep 1.) in
    let b = Miou.call_cc (fun () -> sleep 2.) in
    Miou.await_all [ a; b ]
    |> List.iter @@ function Ok () -> () | Error exn -> raise exn
  in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)
