
let () = Random.self_init ()

exception Basic_failure

let prgm () =
  Miouu.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let child_of_p = Miou.call_cc (fun () -> Miouu.sleep 1.) in
    if Random.bool () then raise Basic_failure;
    Miou.await_exn child_of_p
  in
  Miou.await p

let () =
  let t0 = Unix.gettimeofday () in
  match prgm () with
  | Ok () ->
      let t1 = Unix.gettimeofday () in
      assert (t1 -. t0 >= 1.)
  | Error Basic_failure ->
      let t1 = Unix.gettimeofday () in
      assert (t1 -. t0 < 1.)
  | _ -> failwith "t04"
