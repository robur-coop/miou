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

let rec produce t n max =
  Bounded_stream.put t n;
  if n < max then produce t (succ n) max

let rec consume t cur max =
  let n = Bounded_stream.get t in
  if n <> cur then failwith "consume"
  else if n = max then ()
  else consume t (succ cur) max

let perform = function
  | `Produce (q, min, max) -> produce q min max
  | `Consume (q, cur, max) -> consume q cur max

let () =
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
  List.iter (function Ok v -> v | Error exn -> raise exn) results
