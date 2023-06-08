let () = Random.self_init ()
let with_lock ~m fn = Mutex.lock m; fn (); Mutex.unlock m

let philosopher uid ma mb mo =
  let rec go () =
    let duration = 2 + Random.int 6 in
    with_lock ~m:mo (fun () ->
        Format.printf "%02d thinks %01ds\n%!" uid duration);
    Unix.sleep duration;
    with_lock ~m:mo (fun () -> Format.printf "%02d is hungry\n%!" uid);
    with_lock ~m:ma (fun () ->
        with_lock ~m:mb (fun () ->
            let duration = 2 + Random.int 6 in
            with_lock ~m:mo (fun () ->
                Format.printf "%02d eats %01ds\n%!" uid duration);
            Unix.sleep duration));
    go ()
  in
  go

open Miou

let () =
  let ts = int_of_string "30" in
  Miou.run @@ fun () ->
  let m1 = Mutex.create ()
  and m2 = Mutex.create ()
  and m3 = Mutex.create ()
  and m4 = Mutex.create ()
  and m5 = Mutex.create () in
  let mo = Mutex.create () in
  let uid01 = Prm.call (philosopher 01 m1 m2 mo) in
  let uid02 = Prm.call (philosopher 02 m2 m3 mo) in
  let uid03 = Prm.call (philosopher 03 m3 m4 mo) in
  let uid04 = Prm.call (philosopher 04 m4 m5 mo) in
  let uid05 = Prm.call (philosopher 05 m1 m5 mo) in
  let sleep = Prm.call (fun () -> Unix.sleep ts) in
  Prm.await_first_exn [ uid01; uid02; uid03; uid04; uid05; sleep ]
