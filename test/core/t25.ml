(* NOTE(dinosaure): This code ensure that a task always run into another domain
   than the launcher. We run this test 1000 times to be sure about this
   assertion. *)

let prgm () =
  let p =
    Miou.call @@ fun () ->
    let v = Miou.Domain.self () in
    let q = Miou.call @@ fun () -> Miou.Domain.self () in
    (v, Miou.await_exn q)
  in
  let v, u = Miou.await_exn p in
  assert (v <> u)

let () =
  for _ = 0 to 100 do
    Miou.run prgm
  done
