(* (c) Lwt authors *)

type 'a t = { g: Random.State.t; mutable prev: 'a t; mutable next: 'a t }

type 'a node = {
    g: Random.State.t
  ; mutable prev: 'a t
  ; mutable next: 'a t
  ; mutable data: 'a
  ; mutable active: bool
}

external t_of_node : 'a node -> 'a t = "%identity"
external node_of_t : 'a t -> 'a node = "%identity"

exception Empty

let make g =
  let rec t = { g; prev= t; next= t } in
  t

let remove node =
  if node.active then begin
    node.active <- false;
    let t = t_of_node node in
    t.prev.next <- t.next;
    t.next.prev <- t.prev
  end

let is_empty (t : 'a t) = t.next == t
let data { data; _ } = data

let add_l data (t : 'a t) =
  let node = { g= t.g; prev= t; next= t.next; data; active= true } in
  t.next.prev <- t_of_node node;
  t.next <- t_of_node node

let add_r data (t : 'a t) =
  let node = { g= t.g; prev= t.prev; next= t; data; active= true } in
  t.prev.next <- t_of_node node;
  t.prev <- t_of_node node

let push data (t : 'a t) =
  if Random.State.bool t.g then add_l data t else add_r data t

let length t =
  let rec go curr len =
    if curr == t then
      len
    else
      let node = node_of_t curr in
      go node.next (len + 1)
  in
  go t.next 0

(* NOTE(dinosaure): [take_{r,l}] are unsafe. *)

let[@warning "-32"] take_l (t : 'a t) =
  let node = node_of_t t.next in
  remove node; node.data

let[@warning "-32"] take_r (t : 'a t) =
  let node = node_of_t t.prev in
  remove node; node.data

let take t =
  if is_empty t then
    raise Empty
  else
    let nth = Random.State.int t.g (length t) in
    let rec go cur = function
      | 0 ->
          let node = node_of_t cur in
          remove node; node.data
      | n -> go cur.next (pred n)
    in
    go t.next nth

let iter ~f t =
  let rec go cur =
    if cur != t then begin
      let node = node_of_t cur in
      if node.active then f node.data;
      go node.next
    end
  in
  go t.next

let iter_on ~f t =
  let rec go cur =
    if cur != t then begin
      let node = node_of_t cur in
      if node.active then f node;
      go node.next
    end
  in
  go t.next
