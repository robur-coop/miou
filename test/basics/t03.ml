(* NOTE(dinosaure): This code is a simple producer/consumer - the test exists
   also into the OCaml distribution. *)

type 'a p = {
    buffer: 'a array
  ; mutable rdpos: int
  ; mutable wrpos: int
  ; lock: Mutex.t
  ; non_empty: Miou_unix.Cond.t
  ; non_full: Miou_unix.Cond.t
}

let make size v =
  let lock = Mutex.create () in
  {
    buffer= Array.make size v
  ; lock
  ; rdpos= 0
  ; wrpos= 0
  ; non_empty= Miou_unix.Cond.make ~mutex:lock ()
  ; non_full= Miou_unix.Cond.make ~mutex:lock ()
  }

let put t data =
  let predicate () = (t.wrpos + 1) mod Array.length t.buffer = t.rdpos in
  let fn () =
    t.buffer.(t.wrpos) <- data;
    t.wrpos <- (t.wrpos + 1) mod Array.length t.buffer;
    Miou_unix.Cond.signal t.non_empty
  in
  Miou_unix.Cond.until ~predicate ~fn t.non_full

let get t =
  let predicate () = t.wrpos = t.rdpos in
  let fn () =
    let data = t.buffer.(t.rdpos) in
    t.rdpos <- (t.rdpos + 1) mod Array.length t.buffer;
    Miou_unix.Cond.signal t.non_full;
    data
  in
  Miou_unix.Cond.until ~predicate ~fn t.non_empty

let rec produce t n max =
  let () = put t n in
  if n < max then produce t (succ n) max

let rec consume t cur max =
  let n = get t in
  if n <> cur then failwith "consume"
  else if n = max then ()
  else consume t (succ cur) max

let perform = function
  | `Produce (q, min, max) -> produce q min max
  | `Consume (q, cur, max) -> consume q cur max

let () =
  Miou_unix.run ~domains:3 @@ fun () ->
  let t0 = make 20 0 and t1 = make 30 0 in
  let ress =
    let prm = Miou.call_cc @@ fun () -> consume t1 0 8000 in
    Miou.await prm
    :: Miou.parallel perform
         [
           `Produce (t0, 0, 10000); `Produce (t1, 0, 8000)
         ; `Consume (t0, 0, 10000)
         ]
  in
  List.iter (function Ok v -> v | Error exn -> raise exn) ress
