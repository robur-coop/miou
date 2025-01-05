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

module Trigger : sig
  type t

  val create : unit -> t
  (** [create ()] allocates a new trigger in the initial state. *)

  val is_initial : t -> bool
  (** [is_initial t] dtermines whether the trigger [t] is in the initial state.
  *)

  val is_signaled : t -> bool
  (** [is_signaled t] determines whether the trigger [t] is in the signaled
      state. *)

  val await : t -> (exn * Printexc.raw_backtrace) option
  (** [await t] waits for the trigger to be {!signal}ed.

      The return value is [None] in case the trigger was signaled before [await]
      or we resumed the underlying continuation normally. Otherwise, the return
      value is [Some (exn, bt)], which indicates that the underlying
      continuation has been cancelled and the caller should raise the exception.
      In either case, the caller is responsible for cleaning up. Usually this
      means making sure that no references to the trigger remain to avoid space
      leak.

      @raise Invalid_argument
        if the trigger was in the awaiting state, which means that multiple
        concurrent calls of [await] are being made. *)

  type _ Effect.t +=
    private
    | Await : t -> (exn * Printexc.raw_backtrace) option Effect.t

  val signal : t -> unit
  (** After [signal t] returns, the trigger has been put into the signaled state
      and any attached action (via {!val:on_signal}) has been called.

      Note that under normal circumstance, [signal] should never raise an
      exception. If an exception is raised by [signal], it means that the
      attached action raised an error. *)

  val on_signal : t -> 'x -> 'y -> (t -> 'x -> 'y -> unit) -> bool
  (** [on_signal t x y fn] attempts to attach the [fn] action to the trigger [t]
      and transition the trigger to the awaiting state. It must be safe to call
      [fn t x y] from any context that {!signal} might be called from.

      The return value is [true] in case the action was attached successfully.
      Otherwise, the return value is [false], which means that the trigger was
      already in the signaled state.

      @raise Invalid_argument
        if the trigger was in the awaiting state, which means that either the
        owner or creator of the trigger made concurrent calls to {!val:await} or
        the handler called [on_signal] more than once. *)
end

module Computation : sig
  type 'a state =
    | Cancelled of exn * Printexc.raw_backtrace
    | Returned of 'a
    | Continue of { balance: int; triggers: Trigger.t list }

  type !'a t = private 'a state Atomic.t

  val create : unit -> 'a t
  (** [create ()] creates a new computation in the running state. *)

  val try_return : 'a t -> 'a -> bool
  (** [try_return c value] attempts to complete the computation with the
      specified [value] and returns [true] on success. Otherwise returns
      [false], which means that the computation had already been completed
      before. *)

  val try_capture : 'a t -> ('b -> 'a) -> 'b -> bool
  (** [try_capture c fn x] calls [fn x] and tries to complete the computation
      with the value returned or the exception raised by the call and returns
      [true] on success. Otherwise returns [false], which means that the
      computation had already been completed before. *)

  val try_cancel : 'a t -> exn * Printexc.raw_backtrace -> bool
  (** [try_cancel c (exn, bt)] attempts to mark the computation [c] as cancelled
      with the specified exception and backtrace and returns [true] on success.
      Otherwise returns [false], which means that the computation had already
      been completed before. *)

  val is_running : 'a t -> bool
  (** [is_running c] determines whether the computation [c] is in the running
      state meaning that it has not yet been completed. *)

  val cancelled : 'a t -> (exn * Printexc.raw_backtrace) option
  (** [cancelled c] returns the exception that the computation has been
      cancelled with or returns [None] in case the computation has not been
      cancelled. *)

  val raise_if_errored : 'a t -> unit
  (** [raise_if_errored] raises the exception the computation was cancelled with
      if it was errored. *)

  val peek : 'a t -> ('a, exn * Printexc.raw_backtrace) result option
  (** [peek c] returns the result of the computation [c] or [None] in case the
      computation has not completed. *)

  val try_attach : 'a t -> Trigger.t -> bool
  (** [try_attach c trigger] tries to attach the trigger to be signaled on
      completion of the computation and returns [true] on success. Otherwise, it
      returns [false], which means that the computation has already been
      completed or the trigger has already been signaled. *)

  val detach : 'a t -> Trigger.t -> unit
  (** [detach c trigger] {{!Trigger.signal} signals} the trigger and detaches it
      from the computation [c]. *)

  val clean : 'a t -> unit

  val await : 'a t -> ('a, exn * Printexc.raw_backtrace) result
  (** [await c] waits for the computation to complete and either returns the
      value of the completed computation or the exception the computation was
      cancelled with. *)

  val await_exn : 'a t -> 'a
  val canceller : from:'a t -> into:'b t -> Trigger.t
end
