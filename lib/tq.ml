(* NOTE(dinosaure): [eio] uses the same data-structure but with some
   [Obj.magic]. I don't want to play a game where I'm smarter than OCaml but
   pragmatically, [flambda] exists and can break some assumptions. As God said,
   never use [Obj.magic].

   The paper is available here:
   https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf *)

type 'a t = { tail: 'a node Atomic.t; head: 'a node Atomic.t }
and 'a node = { mutable value: 'a; next: 'a node option Atomic.t }

(* enqueue(x):
     q <- new record
     q.value <- x
     q.next <- NULL
     repeat:
       p <- tail
       succ <- compare_and_swap p.next NULL q
       if succ != true:
         compare_and_swap tail p p.next
       until succ = true
     compare_and_swap tail p q
*)
let enqueue t value =
  let q = { value; next= Atomic.make None } in
  let rec go () =
    let p = Atomic.get t.tail in
    if Atomic.compare_and_set p.next None (Some q) then
      ignore (Atomic.compare_and_set t.tail p q)
    else
      (* XXX(dinosaure): it's safe because our previous [compare_and_set] proved
         that [p.next != None]. *)
      let[@warning "-8"] (Some next) = Atomic.get p.next in
      let _ = Atomic.compare_and_set t.tail p next in
      go ()
  in
  go ()

exception Empty

(* dequeue():
   repeat:
     p <- head
     if p.next == NULL:
       error queue empty
     until compare_and_swap head p p.next
     return p.next.value
*)
let dequeue t =
  let rec go () =
    let p = Atomic.get t.head in
    match Atomic.get p.next with
    | None -> raise Empty
    | Some next ->
        if Atomic.compare_and_set t.head p next then
          let[@warning "-8"] (Some node) = Atomic.get p.next in
          node.value
        else go ()
  in
  go ()

let make () =
  let dummy = { value= Obj.magic (); next= Atomic.make None } in
  let t = { tail= Atomic.make dummy; head= Atomic.make dummy } in
  assert (Atomic.get t.head == Atomic.get t.tail);
  t

let is_empty t =
  let p = Atomic.get t.head in
  match Atomic.get p.next with None -> true | Some _ -> false

let iter ~f t =
  let rec go p =
    match Atomic.get p.next with
    | None -> ()
    | Some next -> f next.value; go next
  in
  go (Atomic.get t.head)

let length t =
  let v = Atomic.make 0 in
  iter ~f:(fun _ -> Atomic.incr v) t;
  Atomic.get v

let drop ~f t =
  let rec go () =
    match dequeue t with v -> f v; go () | exception Empty -> ()
  in
  go ()
