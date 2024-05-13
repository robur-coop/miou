[@@@ocamlformat "disable"]

(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2023 --  Inria - CNRS - Paris-Saclay University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(** This module implements a priority queue based on a minimal binary heap.
    The heap is implemented as a dynamic array, taken from the module vector. *)

(** This is a contribution by Aymeric Walch. *)

(*@ use Order *)
(*@ use Bag *)

module Make (X: sig
  type t

  val dummy : t

  (*@ function cmp : t -> t -> int *)

  (*@ axiom is_pre_order: Order.is_pre_order cmp *)

  val compare : t -> t -> int
    (*@ r = compare x y
          ensures r = cmp x y *)
end) : sig

  type elt = X.t

  type t
  (*@ ephemeral *)
  (*@ mutable model bag : X.t bag *)
  (*@ invariant card bag <= Sys.max_array_length *)

  (*@ predicate mem (x: elt) (h: t) := nb_occ x h.bag > 0 *)

  val create : unit -> t
  (*@ h = create ()
      ensures h.bag = empty_bag  *)

  val is_empty : t -> bool
  (*@ b = is_empty h
      ensures b <-> h.bag = empty_bag *)

  val size : t -> int
  (* x = size h
      ensures x = card h.bag *)

  (*@ function minimum: t -> elt *)

  (*@ predicate is_minimum (x: elt) (h: t) :=
        mem x h && forall e. mem e h -> X.cmp x e <= 0 *)

  (*@ axiom min_def:
        forall h. 0 < card h.bag -> is_minimum (minimum h) h *)

  val find_min : t -> elt option
  (*@ r = find_min h
      ensures match r with
      | None   -> card h.bag = 0
      | Some x -> card h.bag > 0 && x = minimum h *)

  exception Empty

  val find_min_exn : t -> elt
  (*@ x = find_min_exn h
      raises  Empty -> card h.bag = 0
      ensures card h.bag > 0 && x = minimum h *)

  val delete_min_exn : t -> unit
  (*@ delete_min_exn h
      modifies h
      raises  Empty -> card h.bag = 0 && h.bag = old h.bag
      ensures (old h).bag = add (minimum (old h)) h.bag *)

  val extract_min_exn : t -> elt
  (*@ x = extract_min_exn h
      modifies h
      raises  Empty -> card h.bag = 0 && h.bag = old h.bag
      ensures x = minimum (old h)
      ensures (old h).bag = add x h.bag *)

  val insert : elt -> t -> unit
   (*@ insert x h
       checks   card h.bag < Sys.max_array_length
       modifies h
       ensures  h.bag = add x (old h).bag *)

  val iter : (elt -> unit) -> t -> unit
end
