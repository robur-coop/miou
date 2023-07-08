open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Unix.sleepf 1.) in
  let b = Prm.call_cc (fun () -> Unix.sleepf 2.) in
  Prm.await_all [ a; b ] |> ignore

let () =
  Format.printf "call_cc + Unix.sleepf:%!";
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 3.);
  Format.printf " ok\n%!"

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call (fun () -> Unix.sleepf 1.) in
  let b = Prm.call (fun () -> Unix.sleepf 2.) in
  Prm.await_all [ a; b ] |> ignore

let () =
  Format.printf "call + Unix.sleepf:   %!";
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.);
  Format.printf " ok\n%!"

let prgm () =
  Miouu.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Miouu.sleep 1.) in
  let b = Prm.call_cc (fun () -> Miouu.sleep 2.) in
  Prm.await_all [ a; b ] |> ignore

let () =
  Format.printf "call_cc + Miouu.sleep:%!";
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.);
  Format.printf " ok\n%!"
