let () = Printexc.record_backtrace true

module Atomic = Stdlib.Atomic

let test01 =
  let description =
    {text|A simple test to show up the order in which tasks are performed.|text}
  in
  Test.test ~title:"test01" ~description @@ fun () ->
  let buf = Buffer.create 0x100 in
  let print_endline str =
    Buffer.add_string buf str; Buffer.add_string buf "\n"
  in
  Miou.run @@ fun () ->
  let prm = Miou.async @@ fun () -> print_endline "Hello" in
  print_endline "World";
  Miou.await_exn prm;
  Test.check (Buffer.contents buf = "World\nHello\n")

let test02 =
  let description =
    {text|A test to show up our exception Still_has_children and our verification about pending children.|text}
  in
  Test.test ~title:"test02" ~description @@ fun () ->
  let prgm () = ignore (Miou.async (Fun.const ())) in
  try Miou.run prgm; Test.check false
  with exn -> Test.check (Printexc.to_string exn = "Miou.Still_has_children")

let test03 =
  let description =
    {text|A test to show up that we can await multiple times a promise.|text}
  in
  Test.test ~title:"test03" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let prm = Miou.async (Fun.const ()) in
  Miou.await_exn prm; Miou.await_exn prm; Test.check true

let test04 =
  let description = {text|A simple test about parallel tasks|text} in
  Test.test ~title:"test04" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let prm = Miou.call (Fun.const ()) in
  Miou.await_exn prm; Test.check true

let test05 =
  let description = {text|A simple test about Miou.await_one|text} in
  Test.test ~title:"test05" ~description @@ fun () ->
  let prgm () =
    let p0 = Miou.call (Fun.const `P0) in
    let p1 = Miou.call (Fun.const `P1) in
    match Miou.await_one [ p0; p1 ] with
    | Ok `P0 ->
        let p1 = Miou.await_exn p1 in
        assert (p1 = `P1)
    | Ok `P1 ->
        let p0 = Miou.await_exn p0 in
        assert (p0 = `P0)
    | Error exn -> raise exn
  in
  for _ = 0 to 100 do
    Miou.run prgm
  done;
  Test.check true

let test06 =
  let description =
    {text|A test to show up that even if a task has finished, if it is cancelled, it is marked as cancelled.|text}
  in
  Test.test ~title:"test06" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let prm = Miou.async (Fun.const ()) in
  Miou.await_exn prm;
  Miou.cancel prm;
  match Miou.await prm with
  | Error Miou.Cancelled -> Test.check true
  | _ -> Test.check false

let test07 =
  let description =
    {text|Exactly the same test as test06 but with a parallel task.|text}
  in
  Test.test ~title:"test07" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let prm = Miou.call (Fun.const ()) in
  Miou.cancel prm;
  match Miou.await prm with
  | Error Miou.Cancelled -> Test.check true
  | Error _exn -> Test.check false
  | Ok () -> Test.check false

let test08 =
  let description =
    {text|A test to show that in an abnormal situation, you can "forget" your children.|text}
  in
  Test.test ~title:"test08" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let prm =
    Miou.async @@ fun () ->
    let _ = Miou.async (Fun.const ()) in
    Miou.yield (); failwith "t08"
  in
  match Miou.await prm with
  | Error (Failure v) -> Test.check (v = "t08")
  | _ -> Test.check false

let rec infinite () = infinite (Miou.yield ())

let test09 =
  let description = {text|A test to show up the cancellation.|text} in
  Test.test ~title:"test09" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let prm =
    Miou.call @@ fun () ->
    let p0 = Miou.async infinite in
    let p1 = Miou.async infinite in
    Miou.await_exn p0; Miou.await_exn p1
  in
  Miou.cancel prm; Test.check true

let test10 =
  let description =
    {text|A simple test to show up the order in which tasks are performed with Miou.yield.|text}
  in
  Test.test ~title:"test10" ~description @@ fun () ->
  let buf = Buffer.create 0x100 in
  let print_endline str =
    Buffer.add_string buf str; Buffer.add_string buf "\n"
  in
  Miou.run @@ fun () ->
  let prm = Miou.async @@ fun () -> print_endline "Hello" in
  Miou.yield ();
  print_endline "World";
  Miou.await_exn prm;
  Test.check (Buffer.contents buf = "Hello\nWorld\n")

let test11 =
  let description =
    {text|A simple test about Miou.await_first (and cancellation)|text}
  in
  Test.test ~title:"test11" ~description @@ fun () ->
  let prgm () =
    let a = Miou.call (Fun.const 0) in
    let b = Miou.call (Fun.const 1) in
    Miou.await_first [ a; b ]
  in
  let rec until_its value = if Miou.run prgm <> value then until_its value in
  until_its (Ok 0); until_its (Ok 1); Test.check true

let test12 =
  let description =
    {text|A test to show that we can wait only for our direct children.|text}
  in
  Test.test ~title:"test12" ~description @@ fun () ->
  let prgm () =
    let a = Miou.async (Fun.const ()) in
    let b = Miou.async @@ fun () -> Miou.await_exn a in
    Miou.await_exn a; Miou.await_exn b
  in
  try Miou.run prgm; Test.check false
  with exn when Printexc.to_string exn = "Miou.Not_a_child" -> Test.check true

(* NOTE(dinosaure): this test can emit a TSan warning. *)
let test13 =
  let description =
    {text|A test to show that the transmission of a value between tasks only takes place between parents and children.|text}
  in
  Test.test ~title:"test13" ~description @@ fun () ->
  let prgm () =
    let p = ref None in
    let a =
      Miou.call @@ fun () ->
      p := Some (Miou.async (Fun.const ()));
      Miou.await_exn (Option.get !p)
    in
    let b =
      Miou.call @@ fun () ->
      let rec until () =
        if Option.is_some !p then Option.get !p else until (Miou.yield ())
      in
      Miou.await_exn (until ())
    in
    Miou.await_exn a; Miou.await_exn b
  in
  try Miou.run prgm; Test.check false
  with exn when Printexc.to_string exn = "Miou.Not_a_child" -> Test.check true

let test14 =
  let description =
    {text|A test to show our exception No_domain_available|text}
  in
  Test.test ~title:"test14" ~description @@ fun () ->
  let prgm () =
    let p = Miou.call (Fun.const ()) in
    Miou.await_exn p
  in
  try Miou.run ~domains:0 prgm; Test.check false
  with Miou.No_domain_available -> Test.check true

let test15 =
  let description =
    {text|Same as test12 but the order of promises changes and we get Miou.Still_hash_children instead of Miou.Not_a_child.|text}
  in
  Test.test ~title:"test15" ~description @@ fun () ->
  let prgm () =
    let p0 = Miou.async @@ fun () -> Miou.async (Fun.const ()) in
    let p1 = Miou.await_exn p0 in
    Miou.await_exn p1
  in
  try Miou.run prgm; Test.check false
  with exn -> Test.check (Printexc.to_string exn = "Miou.Still_has_children")

let test16 =
  let description =
    {text|A test to show up that when you launch a parallel task, it is actually parallel to the parent.|text}
  in
  Test.test ~title:"test16" ~description @@ fun () ->
  let prgm () =
    let p =
      Miou.call @@ fun () ->
      let v = Stdlib.Domain.self () in
      let q = Miou.call @@ fun () -> Stdlib.Domain.self () in
      (v, Miou.await_exn q)
    in
    let v, u = Miou.await_exn p in
    assert (v <> u)
  in
  for _ = 0 to 100 do
    Miou.run prgm
  done;
  Test.check true

let test17 =
  let description = {text|Test about unhandled effects.|text} in
  Test.test ~title:"test17" ~description @@ fun () ->
  let module T = struct
    type _ Effect.t += Foo : unit Effect.t
  end in
  let prgm () =
    try Effect.perform T.Foo with Effect.Unhandled T.Foo -> failwith "t17"
  in
  try Miou.run prgm; Test.check false
  with Effect.Unhandled T.Foo -> Test.check true

let test18 =
  let description =
    {text|A test to show up that we can Miou.await_one and Miou.await_first the same task.|text}
  in
  Test.test ~title:"test18" ~description @@ fun () ->
  let or_raise = function Ok v -> v | Error exn -> raise exn in
  Miou.run @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  or_raise (Miou.await_one [ p; p ]);
  or_raise (Miou.await_first [ p; p ]);
  Test.check true

let test19 =
  let description =
    {text|A test to show up that we need, at least, 2 domains to launch anywhere parallel tasks.|text}
  in
  Test.test ~title:"test19" ~description @@ fun () ->
  let prgm () =
    let prm =
      Miou.call @@ fun () -> Miou.await_exn (Miou.call (Fun.const ()))
    in
    Miou.await_exn prm
  in
  try Miou.run ~domains:1 prgm; Test.check false
  with Miou.No_domain_available -> Test.check true

let test20 =
  let description =
    {text|A test to show up the cancellation of children for abnormal situation.|text}
  in
  Test.test ~title:"test20" ~description @@ fun () ->
  let rec infinite () = infinite (Miou.yield ()) in
  Miou.run @@ fun () ->
  let prm =
    Miou.call @@ fun () ->
    let _ = Miou.call infinite in
    failwith "foo"
  in
  match Miou.await prm with
  | Error (Failure foo) -> Test.check (foo = "foo")
  | _ -> failwith "t20"

let test21 =
  let description =
    {text|A test to show up that the unique ID of the domain given by Miou is the same than Stdlib.Domain.self.|text}
  in
  Test.test ~title:"test21" ~description @@ fun () ->
  let prgm () =
    let prm =
      Miou.call @@ fun () ->
      let runner = Miou.Domain.Uid.of_int (Stdlib.Domain.self () :> int) in
      let self = Miou.Domain.self () in
      assert (Miou.Domain.Uid.equal runner self)
    in
    Miou.await_exn prm
  in
  for _ = 0 to 100 do
    Miou.run prgm
  done;
  Test.check true

module Miouc = struct
  module Logs = Miou.Logs.Make (struct
    let src = "core"
  end)

  let tick = Atomic.make 0

  type elt = { syscall: Miou.syscall; time: int; mutable cancelled: bool }

  module Heapq = Miou.Pqueue.Make (struct
    type t = elt

    let dummy = { time= 0; syscall= Obj.magic (); cancelled= false }
    let compare { time= a; _ } { time= b; _ } = Int.compare a b
  end)

  type domain = Heapq.t

  let domain =
    let make () = Heapq.create () in
    let domain = Stdlib.Domain.DLS.new_key make in
    fun () -> Stdlib.Domain.DLS.get domain

  let sleep v =
    let syscall = Miou.syscall () in
    let time = Atomic.get tick + v in
    let sleeper = { time; syscall; cancelled= false } in
    Heapq.insert sleeper (domain ());
    Logs.debug (fun m -> m "sleeps until %dc (now: %dc)" time (Atomic.get tick));
    Miou.suspend syscall

  let in_the_past t = t = 0 || t <= Atomic.get tick

  let rec collect signals =
    let heapq = domain () in
    match Heapq.find_min_exn heapq with
    | exception Heapq.Empty -> signals
    | { cancelled= true; _ } -> Heapq.delete_min_exn heapq; collect signals
    | { time; syscall; _ } when in_the_past time ->
        Logs.debug (fun m -> m "signals [%d]" (Miou.uid syscall :> int));
        Heapq.delete_min_exn heapq;
        collect (Miou.signal syscall :: signals)
    | _ -> signals

  let equal (uid : Miou.uid) (uid' : Miou.uid) =
    Int.equal (uid :> int) (uid' :> int)

  let cancel uids =
    let f ({ syscall; _ } as elt) =
      if List.exists (equal (Miou.uid syscall)) uids then elt.cancelled <- true
    in
    Heapq.iter f (domain ())

  let rec next () =
    let heapq = domain () in
    match Heapq.find_min_exn heapq with
    | exception Heapq.Empty -> None
    | { cancelled= true; _ } -> Heapq.delete_min_exn heapq; next ()
    | { time; _ } -> Some time

  let consume_intr fd =
    let buf = Bytes.create 0x100 in
    ignore (Unix.read fd buf 0 (Bytes.length buf))

  let select _uid interrupt ~block:_ cancelled_syscalls =
    cancel cancelled_syscalls;
    let c =
      match next () with
      | None -> 0
      | Some v ->
          let delta = v - Atomic.get tick in
          let delta = min 1 delta in
          let delta = max 0 delta in
          delta
    in
    if Atomic.exchange interrupt false = false then
      let _ = Atomic.fetch_and_add tick c in
      let () = Unix.sleepf 0.01 in
      collect []
    else
      let () = Unix.sleepf 0.01 in
      []

  let events uid =
    let value = Atomic.make false in
    let select = select uid value in
    let interrupt () = Atomic.set value true in
    { Miou.interrupt; select; finaliser= Fun.const () }

  let run ?g ?domains fn = Miou.run ~events ?g ?domains fn

  let reset () =
    let heapq = domain () in
    let rec go () =
      match Heapq.extract_min_exn heapq with
      | exception Heapq.Empty -> ()
      | _ -> go ()
    in
    go (); Atomic.set tick 0

  let tick () = Atomic.get tick
end

let test22 =
  let description =
    {text|A test to show up the cancellation of "syscall".|text}
  in
  Test.test ~title:"test22" ~description @@ fun () ->
  let g = Random.State.make_self_init () in
  let exception Basic_failure in
  let prgm () =
    Miouc.reset ();
    Miouc.run @@ fun () ->
    let p =
      Miou.async @@ fun () ->
      let child_of_p = Miou.async @@ fun () -> Miouc.sleep 1 in
      if Random.State.bool g then raise Basic_failure;
      Miou.await_exn child_of_p
    in
    Miou.await p
  in
  let rec until value =
    let value' = prgm () in
    if value' <> value then until value
  in
  until (Ok ());
  Test.check (Miouc.tick () = 1);
  until (Error Basic_failure);
  Test.check (Miouc.tick () = 0)

let test23 =
  let description =
    {text|A test to show up the cancellation even if we are in the sleep mode.|text}
  in
  Test.test ~title:"test23" ~description @@ fun () ->
  let prgm () =
    Miouc.reset ();
    Miouc.run ~domains:1 @@ fun () ->
    let a = Miou.call @@ fun () -> Miouc.sleep 10 in
    Miouc.sleep 1;
    Miou.cancel a;
    match Miou.await a with Error Miou.Cancelled -> () | _ -> failwith "t23"
  in
  prgm ();
  Test.check (Miouc.tick () < 10)

let test24 =
  let description =
    {text|A test to show up that parallel tasks are parallel.|text}
  in
  Test.test ~title:"test24" ~description @@ fun () ->
  let prgm () =
    Miouc.reset ();
    Miouc.run @@ fun () ->
    let results = Miou.parallel Miouc.sleep [ 2; 2 ] in
    Test.check (List.for_all Result.is_ok results)
  in
  prgm ();
  Test.check (Miouc.tick () < 4)

let test25 =
  let description =
    {text|A test to show up that we must await all our children even if they are stuck into a "syscall".|text}
  in
  Test.test ~title:"test25" ~description @@ fun () ->
  let prgm () =
    Miouc.reset ();
    Miouc.run @@ fun () -> ignore (Miou.call @@ fun () -> Miouc.sleep 10)
  in
  match try Ok (prgm ()) with exn -> Error exn with
  | Ok () -> failwith "t25"
  | Error exn ->
      Test.check (Printexc.to_string exn = "Miou.Still_has_children");
      Test.check (Miouc.tick () < 10)

let test26 =
  let description =
    {text|A test to show up concurrent tasks with sleepers.|text}
  in
  Test.test ~title:"test26" ~description @@ fun () ->
  let prgm () =
    Miouc.reset ();
    Miouc.run @@ fun () ->
    let a = Miou.async @@ fun () -> Miouc.sleep 5 in
    let b = Miou.async @@ fun () -> Miouc.sleep 5 in
    Miou.await_all [ a; b ] |> ignore
  in
  prgm ();
  Test.check (Miouc.tick () < 10)

let test27 =
  let description = {text|Same as test24 but with different sleepers.|text} in
  Test.test ~title:"test27" ~description @@ fun () ->
  let prgm () =
    Miouc.reset ();
    Miouc.run @@ fun () -> ignore (Miou.parallel Miouc.sleep [ 2; 4 ])
  in
  prgm ();
  Test.check (Miouc.tick () <= 5)

let test28 =
  let description =
    {text|A test to show up an events function to transmit system events.|text}
  in
  Test.test ~title:"test28" ~description @@ fun () ->
  let global = ref None in
  let select ~block:_ _ =
    match !global with Some value -> [ Miou.signal value ] | None -> []
  in
  let events _ = { Miou.interrupt= ignore; select; finaliser= Fun.const () } in
  let prgm () =
    Miou.run ~events @@ fun () ->
    let p = Miou.syscall () in
    global := Some p;
    Miou.suspend p
  in
  prgm (); Test.check true

let test29 =
  let description = {text|A test which implements the timeout logic.|text} in
  Test.test ~title:"test29" ~description @@ fun () ->
  let exception Timeout in
  let with_timeout cycle fn =
    let p0 = Miou.async fn in
    let p1 = Miou.async @@ fun () -> Miouc.sleep cycle; raise Timeout in
    Miou.await_first [ p0; p1 ]
  in
  Miouc.reset ();
  Miouc.run @@ fun () ->
  match with_timeout 10 (Fun.const ()) with
  | Ok () -> Test.check (Miouc.tick () < 10)
  | Error _ -> failwith "t29"

let test30 =
  let description =
    {text|A test to show up the finalisation of a task even if we cancel it.|text}
  in
  Test.test ~title:"test30" ~description @@ fun () ->
  let rec infinite () = infinite (Miou.yield ()) in
  let prgm () =
    Miou.run @@ fun () ->
    let s = Atomic.make 0 in
    let prm =
      Miou.call @@ fun () ->
      let finally () = Atomic.decr s in
      Atomic.incr s;
      Fun.protect ~finally infinite
    in
    Miou.cancel prm;
    assert (Atomic.get s = 0)
  in
  for _ = 0 to 10 do
    prgm ()
  done;
  Test.check true

module Bounded_stream = struct
  type 'a t = {
      buffer: 'a array
    ; mutable rd_pos: int
    ; mutable wr_pos: int
    ; lock: Miou.Mutex.t
    ; non_empty: Miou.Condition.t
    ; non_full: Miou.Condition.t
  }

  let create size v =
    let lock = Miou.Mutex.create () in
    let non_empty = Miou.Condition.create () in
    let non_full = Miou.Condition.create () in
    {
      buffer= Array.make size v
    ; lock
    ; rd_pos= 0
    ; wr_pos= 0
    ; non_empty
    ; non_full
    }

  let put t data =
    Miou.Mutex.lock t.lock;
    while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
      Miou.Condition.wait t.non_full t.lock
    done;
    t.buffer.(t.wr_pos) <- data;
    t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer;
    Miou.Condition.signal t.non_empty;
    Miou.Mutex.unlock t.lock

  let get t =
    Miou.Mutex.lock t.lock;
    while t.wr_pos = t.rd_pos do
      Miou.Condition.wait t.non_empty t.lock
    done;
    let data = t.buffer.(t.rd_pos) in
    t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer;
    Miou.Condition.signal t.non_full;
    Miou.Mutex.unlock t.lock;
    data
end

let test31 =
  let description = {text|A test which implements a bounded stream.|text} in
  Test.test ~title:"test31" ~description @@ fun () ->
  let rec produce t n max =
    Bounded_stream.put t n;
    if n < max then produce t (succ n) max
  in
  let rec consume t cur max =
    let n = Bounded_stream.get t in
    if n <> cur then failwith "consume"
    else if n = max then ()
    else consume t (succ cur) max
  in
  let perform = function
    | `Produce (q, min, max) -> produce q min max
    | `Consume (q, cur, max) -> consume q cur max
  in
  Miou.run @@ fun () ->
  let t0 = Bounded_stream.create 20 0 and t1 = Bounded_stream.create 30 0 in
  let prm = Miou.async @@ fun () -> consume t1 0 8000 in
  let results =
    Miou.await prm
    :: Miou.parallel perform
         [
           `Produce (t0, 0, 10000); `Produce (t1, 0, 8000)
         ; `Consume (t0, 0, 10000)
         ]
  in
  List.iter (function Ok v -> v | Error exn -> raise exn) results;
  Test.check true

let finally release acquire = (release, acquire)

let[@inline never] ( let@ ) (release, acquire) fn =
  let x = acquire () in
  match fn x with y -> release x; y | exception exn -> release x; raise exn

let test32 =
  let description = {text|Simple test about triggers.|text} in
  Test.test ~title:"test32" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let trigger = Miou.Trigger.create () in
  let@ _ =
    finally Stdlib.Domain.join @@ fun () ->
    Stdlib.Domain.spawn (fun () -> Miou.Trigger.signal trigger)
  in
  match Miou.Trigger.await trigger with
  | None -> Test.check true
  | Some (exn, _) ->
      Format.eprintf "Got an unexpected exception: %S\n%!"
        (Printexc.to_string exn);
      Test.check false
  | exception exn ->
      Format.eprintf "Got an unexpected exception: %S\n%!"
        (Printexc.to_string exn);
      Test.check false

let test33 =
  let description = {text|Simple test about computations.|text} in
  Test.test ~title:"test33" ~description @@ fun () ->
  Miou.run ~domains:0 @@ fun () ->
  let computation = Miou.Computation.create () in
  let@ _ =
    finally Miou.await_exn @@ fun () ->
    Miou.async @@ fun () ->
    let rec fib i =
      Miou.Computation.raise_if_errored computation;
      Miou.yield ();
      if i <= 1 then i else fib (i - 1) + (i - 2)
    in
    ignore (Miou.Computation.try_capture computation fib 80)
  in
  let@ _ =
    finally Miou.await_exn @@ fun () ->
    Miou.async @@ fun () ->
    let exn_bt = (Exit, Printexc.get_callstack 2) in
    ignore (Miou.Computation.try_cancel computation exn_bt)
  in
  try
    let _ : int = Miou.Computation.await_exn computation in
    Test.check false
  with exn -> Test.check (exn = Exit)

let test34 =
  let description = {text|Test about computation & cancellation.|text} in
  Test.test ~title:"test34" ~description @@ fun () ->
  let rec infinite () = infinite (Miou.yield ()) in
  Miou.run ~domains:1 @@ fun () ->
  let prm = Miou.call infinite in
  Miou.cancel prm; Test.check true

let test35 =
  let description = {text|Basic example of ownerships.|text} in
  Test.test ~title:"test35" ~description @@ fun () ->
  let buf = Buffer.create 0x100 in
  let print () = Buffer.add_string buf "Resource released" in
  let prgm () =
    Miou.run @@ fun () ->
    let p =
      Miou.async @@ fun () ->
      let t = Miou.Ownership.create ~finally:print () in
      Miou.Ownership.own t; failwith "p"
    in
    Miou.await_exn p
  in
  match prgm () with
  | () -> failwith "t35"
  | exception Failure p ->
      Test.check (p = "p");
      Test.check (Buffer.contents buf = "Resource released")
  | exception _ -> Test.check false

let test36 =
  let description = {text|Check if Miou releases resources.|text} in
  Test.test ~title:"test36" ~description @@ fun () ->
  let buf = Buffer.create 0x100 in
  let print () = Buffer.add_string buf "Resource released" in
  let prgm () =
    Miou.run @@ fun () ->
    let p =
      Miou.async @@ fun () ->
      let t = Miou.Ownership.create ~finally:print () in
      Miou.Ownership.own t
    in
    Miou.await_exn p
  in
  match prgm () with
  | () -> failwith "t36"
  | exception exn ->
      Test.check (Printexc.to_string exn = "Miou.Resource_leaked");
      Test.check (Buffer.contents buf = "Resource released")

let test37 =
  let description = {text|Basic usage of Miou.Ownership.disown.|text} in
  Test.test ~title:"test37" ~description @@ fun () ->
  let buf = Buffer.create 0x100 in
  let print () = Buffer.add_string buf "Resource released" in
  let prgm () =
    Miou.run @@ fun () ->
    let p =
      Miou.async @@ fun () ->
      let t = Miou.Ownership.create ~finally:print () in
      Miou.Ownership.own t; Miou.Ownership.disown t
    in
    Miou.await_exn p
  in
  prgm ();
  Test.check (Buffer.contents buf = "")

let test38 =
  let description = {text|Basic usage of hooks.|text} in
  Test.test ~title:"test38" ~description @@ fun () ->
  let buf = Buffer.create 0x100 in
  let print_endline str = Buffer.add_string buf str; Buffer.add_char buf '\n' in
  let prgm () =
    Miou.run @@ fun () ->
    let node1 = Miou.Hook.add (fun () -> print_endline "h1") in
    let node2 = Miou.Hook.add (fun () -> print_endline "h2"; failwith "h2") in
    Miou.yield (); Miou.Hook.remove node2; Miou.Hook.remove node1
  in
  prgm ();
  (* - h1 from Miou.Hook.add h2
     - h2 from Miou.yield
     - h1 from Miou.yield
     - h1 from Miou.Hook.remove node2
     - h1 from Miou.Hook.remove node1 *)
  Test.check (Buffer.contents buf = "h1\nh2\nh1\nh1\nh1\n")

let test39 =
  let description =
    {text|Basic usage of hooks (when we remove them in the wrong place).|text}
  in
  Test.test ~title:"test38" ~description @@ fun () ->
  Miou.run @@ fun () ->
  let hook = Miou.Hook.add Fun.id in
  let prm = Miou.call @@ fun () -> Miou.Hook.remove hook in
  match Miou.await prm with
  | Ok () -> failwith "t39"
  | Error (Invalid_argument _) -> Test.check true
  | Error exn -> raise exn

let test40 =
  let description = {text|Basic usage of lazy values.|text} in
  Test.test ~title:"test40" ~description @@ fun () ->
  let prgm () =
    Miou.run @@ fun () ->
    let v = Miou.Lazy.from_fun @@ fun () -> 42 in
    let prm0 = Miou.call @@ fun () -> `P0 (Miou.Lazy.force v) in
    let prm1 = Miou.call @@ fun () -> `P1 (Miou.Lazy.force v) in
    match Miou.await_first [ prm0; prm1 ] with
    | Ok (`P0 42) -> true
    | Ok (`P1 42) -> false
    | Ok _ -> failwith "Unexpected result"
    | Error exn -> raise exn
  in
  let rec until_its value = if prgm () = value then until_its value in
  until_its true; until_its false; Test.check true

let test41 =
  let description = {text|Ownership of orphans|text} in
  Test.test ~title:"text41" ~description @@ fun () ->
  let prgm () =
    Miou.run @@ fun () ->
    let orphans = Miou.orphans () in
    let prm =
      Miou.async ~orphans @@ fun () ->
      let prm = Miou.async ~orphans (Fun.const ()) in
      Miou.await_exn prm
    in
    Miou.await_exn prm
  in
  match Miou.run prgm with
  | _ -> Test.check false
  | exception Invalid_argument str ->
      Test.check (str = "The given orphans is owned by another promise")

let () =
  let tests =
    [
      test01; test02; test03; test04; test05; test06; test07; test08; test09
    ; test10; test11; test12; test13; test14; test15; test16; test17; test18
    ; test19; test20; test21; test22; test23; test24; test25; test26; test27
    ; test28; test29; test30; test31; test32; test33; test34; test35; test36
    ; test37; test38; test39; test40; test41
    ]
  in
  let ({ Test.directory } as runner) =
    Test.runner (Filename.concat (Sys.getcwd ()) "_tests")
  in
  let run idx test =
    let idx = succ idx in
    Format.printf "test%02d: %!" idx;
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
