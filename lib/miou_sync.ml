(* Copyright (c) 2023 Vesa Karvonen

   Permission to use, copy, modify, and/or distribute this software for any purpose
   with or without fee is hereby granted, provided that the above copyright notice
   and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
   FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
   TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
   THIS SOFTWARE.
*)

module Backoff = Miou_backoff

module Trigger : sig
  type state =
    | Signaled
    | Awaiting : (t -> 'x -> 'y -> unit) * 'x * 'y -> state
    | Initial

  and t = state Atomic.t

  val create : unit -> t
  val is_initial : t -> bool
  val is_signaled : t -> bool
  val signal : t -> unit
  val on_signal : t -> 'x -> 'y -> (t -> 'x -> 'y -> unit) -> bool

  type _ Effect.t +=
    private
    | Await : t -> (exn * Printexc.raw_backtrace) option Effect.t

  val await : t -> (exn * Printexc.raw_backtrace) option
end = struct
  type state =
    | Signaled
    | Awaiting : (t -> 'x -> 'y -> unit) * 'x * 'y -> state
    | Initial

  and t = state Atomic.t

  let create () = Atomic.make Initial

  let rec signal t =
    match Atomic.get t with
    | Signaled -> ()
    | Initial ->
        if not (Atomic.compare_and_set t Initial Signaled) then signal t
    | Awaiting (fn, x, y) as seen ->
        if Atomic.compare_and_set t seen Signaled then fn t x y else signal t

  let is_signaled t = Atomic.get t == Signaled
  let is_initial t = Atomic.get t == Initial
  let[@inline never] awaiting () = invalid_arg "Trigger: already awaiting"

  let rec on_signal t x y fn =
    match Atomic.get t with
    | Initial ->
        Atomic.compare_and_set t Initial (Awaiting (fn, x, y))
        || on_signal t x y fn
    | Signaled -> false
    | Awaiting _ -> awaiting ()

  type _ Effect.t += Await : t -> (exn * Printexc.raw_backtrace) option Effect.t

  let await t =
    match Atomic.get t with
    | Signaled -> None
    | Awaiting _ -> awaiting ()
    | Initial -> Effect.perform (Await t)
end

module Computation : sig
  type 'a state =
    | Cancelled of exn * Printexc.raw_backtrace
    | Returned of 'a
    | Continue of { balance: int; triggers: Trigger.t list }

  type !'a t = 'a state Atomic.t

  val create : unit -> 'a t
  val try_return : 'a t -> 'a -> bool
  val try_capture : 'r t -> ('a -> 'r) -> 'a -> bool
  val try_cancel : 'a t -> exn * Printexc.raw_backtrace -> bool
  val is_running : 'a t -> bool
  val cancelled : 'a t -> (exn * Printexc.raw_backtrace) option
  val raise_if_errored : 'a t -> unit
  val peek : 'a t -> ('a, exn * Printexc.raw_backtrace) result option
  val try_attach : 'a t -> Trigger.t -> bool
  val detach : 'a t -> Trigger.t -> unit
  val clean : 'a t -> unit
  val await : 'a t -> ('a, exn * Printexc.raw_backtrace) result
  val await_exn : 'a t -> 'a
  val canceller : from:'a t -> into:'b t -> Trigger.t
end = struct
  type 'a state =
    | Cancelled of exn * Printexc.raw_backtrace
    | Returned of 'a
    | Continue of { balance: int; triggers: Trigger.t list }

  type 'a t = 'a state Atomic.t

  let create () = Atomic.make (Continue { balance= 0; triggers= [] })

  let cancelled t =
    match Atomic.get t with
    | Cancelled (exn, bt) -> Some (exn, bt)
    | Returned _ | Continue _ -> None

  open struct
    let rec gc length triggers = function
      | [] -> Continue { balance= length; triggers }
      | r :: rs ->
          if Trigger.is_signaled r then gc length triggers rs
          else gc (succ length) (r :: triggers) rs
  end

  let rec try_attach backoff t trigger =
    match Atomic.get t with
    | Returned _ | Cancelled _ -> false
    | Continue r as seen ->
        (not (Trigger.is_signaled trigger))
        &&
        let after =
          if 0 <= r.balance then
            Continue { balance= r.balance + 1; triggers= trigger :: r.triggers }
          else gc 1 [ trigger ] r.triggers
        in
        Atomic.compare_and_set t seen after
        || try_attach (Backoff.once backoff) t trigger

  let try_attach t trigger = try_attach Backoff.default t trigger

  let rec detach backoff t =
    match Atomic.get t with
    | Returned _ | Cancelled _ -> ()
    | Continue r as seen ->
        let after =
          if 0 <= r.balance then Continue { r with balance= r.balance - 2 }
          else gc 0 [] r.triggers
        in
        if not (Atomic.compare_and_set t seen after) then
          detach (Backoff.once backoff) t

  let detach t trigger = Trigger.signal trigger; detach Backoff.default t

  let rec clean backoff t =
    match Atomic.get t with
    | Returned _ | Cancelled _ -> ()
    | Continue r as seen ->
        let after = gc 0 [] r.triggers in
        if not (Atomic.compare_and_set t seen after) then
          clean (Backoff.once backoff) t

  let clean t = clean Backoff.default t

  let is_running t =
    match Atomic.get t with
    | Cancelled _ | Returned _ -> false
    | Continue _ -> true

  open struct
    let rec try_terminate backoff t after =
      match Atomic.get t with
      | Returned _ | Cancelled _ -> false
      | Continue r as seen ->
          if Atomic.compare_and_set t seen after then
            let () = List.iter Trigger.signal r.triggers in
            true
          else try_terminate (Backoff.once backoff) t after
  end

  let try_return t value = try_terminate Backoff.default t (Returned value)

  let try_cancel t (exn, bt) =
    try_terminate Backoff.default t (Cancelled (exn, bt))

  let try_capture t fn x =
    match fn x with
    | y -> try_return t y
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        try_cancel t (exn, bt)

  let raise_if_errored t =
    match Atomic.get t with
    | Cancelled (exn, bt) -> Printexc.raise_with_backtrace exn bt
    | Returned _ | Continue _ -> ()

  let peek t =
    match Atomic.get t with
    | Cancelled (exn, bt) -> Some (Error (exn, bt))
    | Returned v -> Some (Ok v)
    | Continue _ -> None

  open struct
    let propagate _ from into =
      match cancelled from with
      | None -> ()
      | Some v -> ignore (try_cancel into v)
  end

  let canceller ~from ~into =
    Atomic.make (Trigger.Awaiting (propagate, from, into))

  let rec await t =
    match Atomic.get t with
    | Returned value -> Ok value
    | Cancelled (exn, bt) -> Error (exn, bt)
    | Continue _ ->
        let trigger = Trigger.create () in
        if try_attach t trigger then (
          match Trigger.await trigger with
          | None -> await t
          | Some (exn, bt) ->
              detach t trigger;
              Error (exn, bt))
        else await t

  let await_exn t =
    match await t with
    | Ok value -> value
    | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt
end
