(* NOTE(dinosaure): We show up basically what is the behavior of [miou] with
   several compositions of [call]/[call_cc] and [sleep]/[Unix.sleepf]. *)

let prgm () =
  Miou.run @@ fun () ->
  let a = Miou.call_cc (fun () -> Unix.sleepf 1.) in
  let b = Miou.call_cc (fun () -> Unix.sleepf 2.) in
  Miou.await_all [ a; b ] |> ignore

let () =
  Format.printf "call_cc + Unix.sleepf:%!";
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 >= 3.);
  Format.printf " ok\n%!"

let prgm () =
  Miou.run @@ fun () -> ignore (Miou.parallel Unix.sleepf [ 1.; 2. ])

let () =
  Format.printf "call + Unix.sleepf:   %!";
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 3.);
  Format.printf " ok\n%!"

let prgm () =
  Miouu.run @@ fun () ->
  let a = Miou.call_cc (fun () -> Miouu.sleep 1.) in
  let b = Miou.call_cc (fun () -> Miouu.sleep 2.) in
  Miou.await_all [ a; b ] |> ignore

let () =
  Format.printf "call_cc + Miouu.sleep:%!";
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 3.);
  Format.printf " ok\n%!"
