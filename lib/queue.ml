(* NOTE(dinosaure): [eio] uses the same data-structure but with some
   [Obj.magic]. I don't want to play a game where I'm smarter than OCaml but
   pragmatically, [flambda] exists and can break some assumptions. As God said,
   never use [Obj.magic].

   The paper is available here:
   https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf *)

type 'a t = { tail: 'a node Atomic.t; head: 'a node Atomic.t }

and 'a node = {
    mutable value: 'a
  ; next: 'a node option Atomic.t
  ; mutable count: int
}

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
  let q = { value; next= Atomic.make None; count= 0 } in
  let rec go () =
    let p = Atomic.get t.tail in
    q.count <- p.count + 1;
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
        if Atomic.compare_and_set t.head p next then (
          let value = next.value in
          next.value <- Obj.magic ();
          (* XXX(dinosaure): it is safe to set the value to [Obj.magic ()]
             (or [NULL]) where this value will be never used then. It fixes a
             memory leak on the queue - indeed, as long as [next] is used (it is
             possible that [tail] still points to it), we keep [value] too, only
             a subsequent function which goes through our queue is able to
             physically delete [next]. *)
          value)
        else go ()
  in
  go ()

let make () =
  let dummy = { value= Obj.magic (); next= Atomic.make None; count= 0 } in
  let t = { tail= Atomic.make dummy; head= Atomic.make dummy } in
  assert (Atomic.get t.head == Atomic.get t.tail);
  t

let is_empty t =
  let p = Atomic.get t.head in
  match Atomic.get p.next with None -> true | Some _ -> false

type 'a snapshot = 'a node * 'a node

(* XXX(dinosaure): [snapshot] returns an **accurate** view of the given queue.
   It merely points to two nodes ([head] and [tail]) in the queue at a point in
   time. For the following operations, it is preferable to use a snapshot rather
   than the queue directly - it can be modified in parallel by another domain.
*)
let rec snapshot t : 'a snapshot =
  let head = Atomic.get t.head and tail = Atomic.get t.tail in
  match Atomic.get tail.next with
  | Some node ->
      let _ = Atomic.compare_and_set t.tail tail node in
      snapshot t
  | None -> if Atomic.get t.head != head then snapshot t else (head, tail)

let length t =
  let head, tail = snapshot t in
  tail.count - head.count

let iter ~f t =
  let head, tail = snapshot t in
  let rec go prev =
    if prev != tail then
      match Atomic.get prev.next with
      | None -> ()
      | Some next -> f next.value; go next
  in
  go head

let rec drop ~f t =
  let head, tail = snapshot t in
  if Atomic.compare_and_set t.head head tail then (
    let rec go prev =
      if prev != tail then
        match Atomic.get prev.next with
        | None -> ()
        | Some next -> f next.value; go next
    in
    go head;
    tail.value <- Obj.magic ())
  else drop ~f t

let to_list t =
  let res = ref [] in
  let f v = res := v :: !res in
  iter ~f t; List.rev !res

let transfer t =
  let q = make () in
  drop ~f:(fun x -> enqueue q x) t;
  q
