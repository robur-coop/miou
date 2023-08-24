open Miou

external reraise : exn -> 'a = "%reraise"

let check test =
  try
    assert test;
    print_string "."
  with exn -> print_string "x"; reraise exn

let () =
  let q = Queue.create () in
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
  let q = Queue.create () in
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
  let q = Queue.create () in
  check (Queue.is_empty q = true)

let () =
  let q = Queue.create () in
  Queue.enqueue q 0;
  check (Queue.is_empty q = false)

let () =
  let q = Queue.create () in
  for i = 1 to 100 do
    Queue.enqueue q i
  done;
  for _ = 1 to 10 do
    ignore (Queue.dequeue q)
  done;
  check (Queue.length q = 90)

let () =
  let p = Queue.create () in
  for i = 1 to 100 do
    Queue.enqueue p i
  done;
  let q = Queue.transfer p in
  (match Queue.dequeue p with
  | _ -> failwith "Queue.transfer / Queue.dequeue"
  | exception Queue.Empty -> check true);
  check (Queue.length p = 0);
  check (Queue.length q = 100)

let () =
  let q = Queue.create () in
  for i = 0 to 9 do
    Queue.enqueue q i
  done;
  let lst = Queue.to_list q in
  List.iteri (fun a b -> check (a = b)) lst

let () = print_endline " ok"
