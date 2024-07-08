open Picos_sync

module Bounded_q : sig
  type 'a t
  val create : capacity:int -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end = struct
  type 'a t = {
    mutex : Mutex.t;
    queue : 'a Queue.t;
    capacity : int;
    not_empty : Condition.t;
    not_full : Condition.t;
  }

  let create ~capacity =
    if capacity < 0 then
      invalid_arg "negative capacity"
    else {
      mutex = Mutex.create ();
      queue = Queue.create ();
      capacity;
      not_empty = Condition.create ();
      not_full = Condition.create ();
    }

  let is_full_unsafe t =
    t.capacity <= Queue.length t.queue

  let push t x =
    let was_empty =
      Mutex.protect t.mutex @@ fun () ->
      while is_full_unsafe t do
        Condition.wait t.not_full t.mutex
      done;
      Queue.push x t.queue;
      Queue.length t.queue = 1
    in
    if was_empty then
      Condition.signal t.not_empty

  let pop t =
    let elem, was_full =
      Mutex.protect t.mutex @@ fun () ->
      while Queue.length t.queue = 0 do
        Condition.wait
          t.not_empty t.mutex
      done;
      let was_full = is_full_unsafe t in
      Queue.pop t.queue, was_full
    in
    if was_full then
      Condition.signal t.not_full;
    elem
end

let () = Miou.run @@ fun () ->
  let bq = Bounded_q.create ~capacity:3 in
  let p = Miou.async @@ fun () ->
    let p = Miou.async @@ fun () ->
      while true do
        Printf.printf "Popped %d\n%!"
          (Bounded_q.pop bq);
      done in

    for i=1 to 5 do
      Printf.printf "Pushing %d\n%!" i;
      Bounded_q.push bq i;
    done;

    Printf.printf "All done?\n%!";
    Miou.cancel p in
  Miou.await_exn p;
  Printf.printf "Pushing %d\n%!" 101;
  Bounded_q.push bq 101;
  Printf.printf "Popped %d\n%!" (Bounded_q.pop bq)
