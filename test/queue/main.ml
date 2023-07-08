open Miou

external reraise : exn -> 'a = "%reraise"

let check test =
  try
    assert test;
    print_string "."
  with exn -> print_string "x"; reraise exn

let () =
  let q = Queue.make () in
  Queue.enqueue q 1;
  Queue.enqueue q 2;
  Queue.enqueue q 3;
  let rec go acc =
    match Queue.dequeue q with
    | v -> go (v :: acc)
    | exception Queue.Empty -> acc
  in
  check (go [] = [ 3; 2; 1 ])

let test01 len =
  let q = Queue.make () in
  let lst = List.init len (fun _ -> Random.bits ()) in
  List.iter (Queue.enqueue q) lst;
  let rec go acc =
    match Queue.dequeue q with
    | v -> go (v :: acc)
    | exception Queue.Empty -> List.rev acc
  in
  check (go [] = lst)

let () = test01 10
let () = test01 100
let () = test01 1000

let () =
  let q = Queue.make () in
  check (Queue.is_empty q = true)

let () =
  let q = Queue.make () in
  Queue.enqueue q 0;
  check (Queue.is_empty q = false)

let () = print_endline " ok"
