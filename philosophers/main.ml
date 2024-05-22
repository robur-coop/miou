module Semaphore = struct
  type t = {
      mutable value: int
    ; mutable wakeups: int
    ; mutex: Miou.Mutex.t
    ; condition: Miou.Condition.t
  }

  let create value =
    {
      value
    ; wakeups= 0
    ; mutex= Miou.Mutex.create ()
    ; condition= Miou.Condition.create ()
    }

  let acquire t =
    Miou.Mutex.lock t.mutex;
    while t.value <= 0 do
      Miou.Condition.wait t.condition t.mutex
    done;
    t.value <- t.value - 1;
    Miou.Mutex.unlock t.mutex

  let release t =
    Miou.Mutex.lock t.mutex;
    t.value <- t.value + 1;
    Miou.Condition.signal t.condition;
    Miou.Mutex.unlock t.mutex
end

let () = Random.self_init ()

let with_lock m fn =
  let finally () = Miou.Mutex.unlock m in
  Miou.Mutex.lock m; Fun.protect ~finally fn

let n = 5
let l i = (i - 1 + n) mod n
let r i = (i + 1) mod n
let output = Miou.Mutex.create ()
let critical = Miou.Mutex.create ()

type state = Hungry | Thinking | Eating

let test sem state i =
  if state.(i) = Hungry && state.(l i) <> Eating && state.(r i) <> Eating then (
    state.(i) <- Eating;
    Semaphore.release sem.(i))

let think i =
  let duration = 1. +. Random.float 5. in
  with_lock output (fun () ->
      Format.printf "%02d is thinking %fs\n%!" i duration);
  Miou_unix.sleep duration

let take_forks sem state i =
  let () =
    with_lock critical @@ fun () ->
    state.(i) <- Hungry;
    with_lock output (fun () -> Format.printf "%02d is hungry\n%!" i);
    test sem state i
  in
  Semaphore.acquire sem.(i)

let eat i =
  let duration = 1. +. Random.float 5. in
  with_lock output (fun () -> Format.printf "%02d is eating\n%!" i);
  Miou_unix.sleep duration

let put_forks sem state i =
  with_lock critical @@ fun () ->
  state.(i) <- Thinking;
  test sem state (l i);
  test sem state (r i)

let philosopher sem state i =
  let rec go () =
    think i; take_forks sem state i; eat i; put_forks sem state i; go ()
  in
  go ()

let () =
  let ts =
    match int_of_string Sys.argv.(1) with value -> value | exception _ -> 30
  in
  Miou_unix.run @@ fun () ->
  let sem = Array.init 5 (fun _ -> Semaphore.create 1) in
  let state = Array.init 5 (fun _ -> Thinking) in
  let sleep =
    Miou.async @@ fun () ->
    Miou_unix.sleep (Float.of_int ts);
    Array.iter Semaphore.release sem
  in
  let philosophers =
    List.init (Stdlib.Domain.recommended_domain_count () - 1) Fun.id
  in
  let philosophers =
    Miou.async @@ fun () ->
    ignore (Miou.parallel (philosopher sem state) philosophers)
  in
  Miou.await_first [ philosophers; sleep ] |> ignore
