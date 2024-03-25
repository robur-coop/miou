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

(** This is OCaml code extracted from a verified WhyML implementation,
    part of the VOCaL library.
    See https://github.com/vocal-project/vocal/ *)

(** This is a contribution by Aymeric Walch. *)

module Make(X: sig type t
  val dummy : t
  val compare : t -> t -> int end) =
struct
  type elt = X.t

  type t = X.t Vector.t

  let create (_: unit) : t =
    Vector.create ?capacity:(Some 0) ~dummy:X.dummy ()

  let is_empty (h: t) : bool = Vector.is_empty h

  let size (h: t) : int = Vector.length h

  exception Empty

  let find_min_exn (h: t) : X.t =
    begin
      if Vector.is_empty h then begin raise Empty end;
      Vector.get h 0
    end

  let find_min (h: t) : X.t option =
    if Vector.is_empty h then begin None end
    else
    begin
      Some (Vector.get h 0) end

  let rec move_down (a: X.t Vector.t) (i: int) (x: X.t) : unit =
    let n = Vector.length a in
    let q = if n = 1 then begin (-1) end else begin (n - 2) / 2 end in
    if i <= q then begin
      let j = let j1 = (2 * i) + 1 in
        if
          ((j1 + 1) < n) && ((X.compare (Vector.get a (j1 + 1))
                                (Vector.get a j1)) < 0) then begin
          j1 + 1 end
        else
        begin
          j1 end in
      if (X.compare (Vector.get a j) x) < 0 then begin
        begin
          let o = Vector.get a j in Vector.set a i o;
          move_down a j x
        end end
      else
      begin
        Vector.set a i x end end
    else
    begin
      Vector.set a i x end

  let extract_min_exn (h: t) : X.t =
    begin try let x = Vector.pop h in
      let n = Vector.length h in
      if not (n = 0) then begin
        let min = Vector.get h 0 in begin move_down h 0 x; min end end
      else
      begin
        x end with
    | Vector.Empty -> raise Empty
    end

  let delete_min_exn (h: t) : unit = ignore (extract_min_exn h)

  let rec move_up (a: X.t Vector.t) (i: int) (x: X.t) : unit =
    if i = 0 then begin Vector.set a i x end
    else
    begin
      let j = (i - 1) / 2 in
      let y = Vector.get a j in
      if (X.compare y x) > 0 then begin
        begin Vector.set a i y; move_up a j x end end
      else
      begin
        Vector.set a i x end end

  let insert (x: X.t) (h: t) : unit =
    begin
      if (size h) = Sys.max_array_length
      then begin
        raise (Invalid_argument "") end;
      let n = Vector.length h in
      if n = 0 then begin Vector.push h x end
      else
      begin
        let j = (n - 1) / 2 in
        let y = Vector.get h j in
        if (X.compare y x) > 0 then begin
          begin Vector.push h y; move_up h j x end end
        else
        begin
          Vector.push h x end end
    end

  let iter f (h: t) = Vector.iter f h
end
