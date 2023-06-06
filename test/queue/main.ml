open Miou

let () = Printexc.record_backtrace true

external reraise : exn -> 'a = "%reraise"

let check test =
  try
    assert test;
    print_string "."
  with exn ->
    print_string "x";
    reraise exn

let () =
  let q = Tq.make () in
  Tq.enqueue q 1;
  Tq.enqueue q 2;
  Tq.enqueue q 3;
  let rec go acc =
    match Tq.dequeue q with v -> go (v :: acc) | exception Tq.Empty -> acc
  in
  check (go [] = [ 3; 2; 1 ])

let test01 len =
  let q = Tq.make () in
  let lst = List.init len (fun _ -> Random.bits ()) in
  List.iter (Tq.enqueue q) lst;
  let rec go acc =
    match Tq.dequeue q with
    | v -> go (v :: acc)
    | exception Tq.Empty -> List.rev acc
  in
  check (go [] = lst)

let () = test01 10
let () = test01 100
let () = test01 1000

let () =
  let q = Tq.make () in
  check (Tq.is_empty q = true)

let () =
  let q = Tq.make () in
  Tq.enqueue q 0;
  check (Tq.is_empty q = false)

let () = print_endline " ok"
