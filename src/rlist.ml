(* (c) Lwt authors *)

type 'a t = { g: Random.State.t; mutable prev: 'a t; mutable next: 'a t }

type 'a node = {
    g: Random.State.t
  ; mutable prev: 'a t
  ; mutable next: 'a t
  ; mutable data: 'a
}

external t_of_node : 'a node -> 'a t = "%identity"
external node_of_t : 'a t -> 'a node = "%identity"

exception Empty

let make g =
  let rec t = { g; prev= t; next= t } in
  t

let remove node =
  let t = t_of_node node in
  t.prev.next <- t.next;
  t.next.prev <- t.prev

let is_empty (t : 'a t) = t.next == t

let add_l data (t : 'a t) =
  let node = { g= t.g; prev= t; next= t.next; data } in
  t.next.prev <- t_of_node node;
  t.next <- t_of_node node

let add_r data (t : 'a t) =
  let node = { g= t.g; prev= t.prev; next= t; data } in
  t.prev.next <- t_of_node node;
  t.prev <- t_of_node node

let push data (t : 'a t) =
  if Random.State.bool t.g then add_l data t else add_r data t

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
  remove node; node.data

let take_r (t : 'a t) =
  let node = node_of_t t.prev in
  remove node; node.data

let take t =
  if is_empty t then raise Empty
  else if Random.State.bool t.g then take_l t
  else take_r t
