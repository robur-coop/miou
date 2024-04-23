(* (c) Lwt authors *)

type 'a t = { mutable prev: 'a t; mutable next: 'a t }

type 'a node = {
    mutable prev: 'a t
  ; mutable next: 'a t
  ; mutable data: 'a
  ; mutable active: bool
}

type direction = Right | Left

external t_of_node : 'a node -> 'a t = "%identity"
external node_of_t : 'a t -> 'a node = "%identity"

exception Empty

let create () =
  let rec t = { prev= t; next= t } in
  t

let remove node =
  if node.active then (
    node.active <- false;
    node.data <- Obj.magic ();
    let t = t_of_node node in
    t.prev.next <- t.next;
    t.next.prev <- t.prev)

let is_empty (t : 'a t) = t.next == t
let[@inline always] data { data; _ } = data

let add_l data (t : 'a t) =
  let node = { prev= t; next= t.next; data; active= true } in
  t.next.prev <- t_of_node node;
  t.next <- t_of_node node

let add_r data (t : 'a t) =
  let node = { prev= t.prev; next= t; data; active= true } in
  t.prev.next <- t_of_node node;
  t.prev <- t_of_node node

let peek_node direction (t : _ t) =
  match direction with Left -> node_of_t t.next | Right -> node_of_t t.prev

let add direction t data =
  match direction with Left -> add_l data t | Right -> add_r data t

let length t =
  let rec go curr len =
    if curr == t then len
    else
      let node = node_of_t curr in
      go node.next (len + 1)
  in
  go t.next 0

(* NOTE(dinosaure): [take_{r,l}] are unsafe. *)

let take_l (t : 'a t) =
  let node = node_of_t t.next in
  let data = node.data in
  remove node; data

let take_r (t : 'a t) =
  let node = node_of_t t.prev in
  let data = node.data in
  remove node; data

let take direction t =
  if is_empty t then raise Empty
  else match direction with Left -> take_l t | Right -> take_r t

let drop t =
  while not (is_empty t) do
    ignore (take_l t)
  done

let exists f t =
  let rec go cur =
    if cur == t then false
    else if f (node_of_t cur).data then true
    else go cur.next
  in
  go t.next

let iter ~f t =
  let rec go cur =
    if cur != t then (
      let node = node_of_t cur in
      if node.active then f node.data;
      go node.next)
  in
  go t.next

let iter_node ~f t =
  let rec go cur =
    if cur != t then (
      let node = node_of_t cur in
      if node.active then f node;
      go node.next)
  in
  go t.next

let to_list t =
  let res = ref [] in
  let f data = res := data :: !res in
  iter ~f t; List.rev !res
