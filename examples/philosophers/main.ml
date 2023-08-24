let () = Random.self_init ()
let with_lock m fn = Mutex.lock m; fn (); Mutex.unlock m
let n = 5
let l i = (i - 1 + n) mod n
let r i = (i + 1) mod n
let output = Mutex.create ()
let critical = Mutex.create ()

type state = Hungry | Thinking | Eating

let test sem state i =
  if state.(i) = Hungry && state.(l i) <> Eating && state.(r i) <> Eating then (
    state.(i) <- Eating;
    Semaphore.Binary.release sem.(i))

let think i =
  let duration = 1. +. Random.float 5. in
  with_lock output (fun () ->
      Format.printf "%02d is thinking %fs\n%!" i duration);
  Miouu.sleep duration

let take_forks sem state i =
  let () =
    with_lock critical @@ fun () ->
    state.(i) <- Hungry;
    with_lock output (fun () -> Format.printf "%02d is hungry\n%!" i);
    test sem state i
  in
  Semaphore.Binary.acquire sem.(i)

let eat i =
  let duration = 1. +. Random.float 5. in
  with_lock output (fun () -> Format.printf "%02d is eating\n%!" i);
  Miouu.sleep duration

let put_forks sem state i =
  with_lock critical @@ fun () ->
  state.(i) <- Thinking;
  test sem state (l i);
  test sem state (r i)

let philosopher sem state i =
  let rec go () =
    think i;
    take_forks sem state i;
    Miou.yield ();
    eat i;
    put_forks sem state i;
    Miou.yield ();
    go ()
  in
  go ()

let () =
  let ts =
    match int_of_string Sys.argv.(1) with value -> value | exception _ -> 30
  in
  Miouu.run @@ fun () ->
  let sem = Array.init 5 (fun _ -> Semaphore.Binary.make false) in
  let state = Array.init 5 (fun _ -> Thinking) in
  let sleep =
    Miou.call_cc @@ fun () ->
    let finally = Array.iter Semaphore.Binary.release in
    let t = Miou.Ownership.own ~finally sem in
    Miouu.sleep (Float.of_int ts);
    finally sem;
    Miou.Ownership.disown t
  in
  let philosophers =
    List.init (Stdlib.Domain.recommended_domain_count () - 1) Fun.id
  in
  let philosophers =
    Miou.call_cc @@ fun () ->
    ignore (Miou.parallel (philosopher sem state) philosophers)
  in
  Miou.yield ();
  Miou.await_first [ philosophers; sleep ] |> ignore
