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

type 'a t = {
  dummy: 'a;
  mutable size: int;
  mutable data: ('a array);
  }

let create ?capacity:(capacity: (int) option) ~dummy:(dummy: 'a) () : 'a t =
  let capacity1 = begin match capacity with
    | None -> 0
    | Some c -> c
    end in
  { dummy = dummy; size = 0; data = (Array.make capacity1 dummy) }

let make ?dummy:(dummy: 'a option) (n: int) (x: 'a) : 'a t =
  let dummy1 = begin match dummy with
    | None -> x
    | Some d -> d
    end in
  { dummy = dummy1; size = n; data = (Array.make n x) }

let init ~dummy:(dummy: 'a) (n: int) (f: (int) -> 'a) : 'a t =
  let a = make  n dummy in
  begin
    let o = n - 1 in
    let o1 = 0 in for i1 = o1 to o do (a.data).(i1) <- (f i1) done; a
  end

let length (a: 'a t) : int = a.size

let get (a: 'a t) (i2: int) : 'a = (a.data).(i2)

let set (a: 'a t) (n: int) (x: 'a) : unit = (a.data).(n) <- x

let unsafe_resize (a: 'a t) (n: int) : unit =
  let n_old = Array.length (a.data) in
  begin
    if n <= (a.size) then begin
      if n < (n_old / 4) then begin
        let o = Array.sub (a.data) 0 n in a.data <- o end
      else
      begin
        Array.fill (a.data) n ((a.size) - n) (a.dummy) end end
    else
    begin
      if n > n_old
      then begin
        let n_div2 = n / 2 in
        let nqt =
          if n_div2 >= n_old then begin
            if (Sys.max_array_length / 2) >= n_div2 then begin n end
            else
            begin
              Sys.max_array_length end end
          else
          begin
            if (Sys.max_array_length / 2) >= n_old then begin 2 * n_old end
            else
            begin
              Sys.max_array_length end end in
        let aqt = Array.make nqt (a.dummy) in
        begin Array.blit (a.data) 0 aqt 0 (a.size); a.data <- aqt end end end;
    a.size <- n
  end

let resize (a: 'a t) (n: int) : unit =
  begin
    if not ((0 <= n) && (n <= Sys.max_array_length))
    then begin
      raise (Invalid_argument "") end;
    unsafe_resize a n
  end

let clear (a: 'a t) : unit = unsafe_resize a 0

let is_empty (a: 'a t) : bool = (length a) = 0

let sub (a: 'a t) (ofs: int) (n: int) : 'a t =
  { dummy = (a.dummy); size = n; data = (Array.sub (a.data) ofs n) }

let fill (a: 'a t) (ofs: int) (n: int) (x: 'a) : unit =
  Array.fill (a.data) ofs n x

let blit (a1: 'a t) (ofs1: int) (a2: 'a t) (ofs2: int) (n: int) : unit =
  Array.blit (a1.data) ofs1 (a2.data) ofs2 n

let append (a1: 'a t) (a2: 'a t) : 'a t =
  let n1 = length a1 in
  let n2 = length a2 in
  let a = make  (n1 + n2) (a1.dummy) in
  begin blit a1 0 a 0 n1; blit a2 0 a n1 n2; a end

let merge_right (a1: 'a t) (a2: 'a t) : unit =
  let n1 = length a1 in
  let n2 = length a2 in
  let size = n1 + n2 in
  begin unsafe_resize a1 size; blit a2 0 a1 n1 n2; clear a2 end

let copy (a1: 'a t) : 'a t =
  { dummy = (a1.dummy); size = (a1.size); data = (Array.copy (a1.data)) }

let push (a: 'a t) (x: 'a) : unit =
  let n = a.size in begin unsafe_resize a (n + 1); (a.data).(n) <- x end

exception Empty

let pop (a: 'a t) : 'a =
  let n = (length a) - 1 in
  begin
    if n < 0 then begin raise Empty end; let r = (a.data).(n) in
    begin unsafe_resize a n; r end
  end

let pop_opt (a: 'a t) : 'a option =
  let n = (length a) - 1 in
  if n < 0 then begin None end
  else
  begin
    let r = (a.data).(n) in begin unsafe_resize a n; Some r end end

let top (a: 'a t) : 'a = let n = length a in (a.data).((n - 1))

let top_opt (a: 'a t) : 'a option =
  let n = length a in
  if n = 0 then begin None end else begin Some ((a.data).((n - 1))) end

let fold_left (a: 'a t) (f: 'b -> ('a -> 'b)) (acc: 'b) : 'b =
  let r = ref acc in
  begin
    let o = (length a) - 1 in
    let o1 = 0 in
    for i2 = o1 to o do let o2 = (f (!r)) (get a i2) in r := o2 done; !r
  end

let fold_right (a: 'a t) (f: 'a -> ('b -> 'b)) (acc: 'b) : 'b =
  let n = length a in
  let r = ref acc in
  begin
    let o = 0 in
    let o1 = n - 1 in
    for i3 = o1 downto o do let o2 = (f (get a i3)) (!r) in r := o2 done; !r
  end

let map ~dummy:(dummy: 'b) (a: 'a t) (f: 'a -> 'b) : 'b t =
  let n = length a in
  let a_new = make  n dummy in
  begin
    let o = n - 1 in
    let o1 = 0 in
    for i4 = o1 to o do let x = get a i4 in (a_new.data).(i4) <- (f x) done;
    a_new
  end

let mapi ~dummy:(dummy: 'b) (a: 'a t) (f: (int) -> ('a -> 'b)) : 'b t =
  let n = length a in
  let a_new = make  n dummy in
  begin
    let o = n - 1 in
    let o1 = 0 in
    for i5 = o1 to o do let x = get a i5 in (a_new.data).(i5) <- ((f i5) x)
      done;
    a_new
  end


let iteri f a = for i = 0 to length a - 1 do f i (get a i) done

let iter f a =
  for i = 0 to length a - 1 do f (get a i) done
