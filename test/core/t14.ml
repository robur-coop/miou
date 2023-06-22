open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Unix.sleepf 0.2) in
  let b = Prm.call_cc (fun () -> Unix.sleepf 0.4) in
  Prm.await_all [ a; b ] |> ignore

let () =
  Format.printf "call_cc + Unix.sleepf:%!";
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.6);
  Format.printf " ok\n%!"

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call (fun () -> Unix.sleepf 0.2) in
  let b = Prm.call (fun () -> Unix.sleepf 0.4) in
  Prm.await_all [ a; b ] |> ignore

let () =
  Format.printf "call + Unix.sleepf:   %!";
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 0.6);
  Format.printf " ok\n%!"

let prgm () =
  Miouu.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Miouu.sleep 0.2) in
  let b = Prm.call_cc (fun () -> Miouu.sleep 0.4) in
  Prm.await_all [ a; b ] |> ignore

let () =
  Format.printf "call_cc + Miouu.sleep:%!";
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 0.6);
  Format.printf " ok\n%!"
