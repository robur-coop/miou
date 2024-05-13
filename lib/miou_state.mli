type ('a, 'b) continuation
(** The type of continuations. [('a, 'b) continuation] is the state of a
    function [_ -> 'b]. ['a] is the type of the value to {i continue}
    the continuation. The user can also {i discontinue} with an exception
    the continuation. *)

type error = exn * Printexc.raw_backtrace
(** The type of errors. A continuation can raise or be cancelled by an
    exception. We keep the backtrace (where the exception comes from) with
    the exception raised/used. *)

(** The type of function states.

    The state of a function is its execution state. A function can finish with
    its return value or an exception, or it can suspend on an {!type:Effect.t}.
    In the case of a suspension, the user can "continue" the execution via what
    is expected by the associated effect. Note that {!val:once} can only be used
    {b once} on a given value {!type:t} (otherwise, an exception
    {!exception:Continuation_already_resumed} is raised by OCaml). *)
type 'a t = private
  | Finished of ('a, error) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t

val pp : 'a t Miou_fmt.t

module Operation : sig
  type 'a t

  val interrupt : _ t
  val continue : 'a Effect.t -> 'a t
  val return : 'a -> 'a t
  val fail : backtrace:Printexc.raw_backtrace -> exn -> _ t
  val perform : 'a Effect.t -> 'a t
  val yield : unit t
end

type ('a, 'b) handler = ('a Operation.t -> 'b t) -> 'a Effect.t -> 'b t

type perform = { perform: 'a 'b. ('a, 'b) handler } [@@unboxed]
(** Type of the effect handler.

    [perform] is a function which should handle incoming effects and give an
    {i operation} {!type:Operation.t} via the given continuation [k]. *)

val make : ('a -> 'b) -> 'a -> 'b t
(** [make fn value] makes a new {i function state} by executing the function
    with the given argument. *)

val suspended_with : ('c, 'a) continuation -> 'c Effect.t -> 'a t
(** [suspended_with k eff] allows you to create a state from the given
    suspension [k] and the effect [eff] which produced the given suspension.
    This function {b does not} continue the given [k], it is safe to use it even
    if the user resumed [k] elsewhere. *)

val once : perform:perform -> 'a t -> 'a t
(** [once ~perform state] applies [perform] once on the given state if the
    latter emits an effect. *)

val fail : backtrace:Printexc.raw_backtrace -> exn:exn -> 'a t -> 'a t
(** [fail ~exn state] discontinues the given state with the given exception. It
    always return [Finished (Error exn)]. If the given state was already resumed
    elsewhere, this function traps the exception [Continuation_already_resumed]
    and return [Finished (Error exn)]. *)

val pure : ('a, error) result -> 'a t
(** [pure value] returns [Finished value]. *)

val run : quanta:int -> perform:perform -> 'a t -> 'a t
(** [run ~quanta ~perform state] applies {!val:once} [quanta] times. If
    [perform] responds with {!val:Operation.interrupt} (and therefore does
    nothing), even though there may be a few {i quanta} left, the function
    returns the last state obtained.

    The same applies to {!val:yield}, except that the continuation has
    burnt itself out. In other words, {!val:yield} is equivalent to
    [send (); interrupt] but costs only one {i quanta}. *)

(**/**)

val continue_with : ('a, 'b) continuation -> 'a -> 'b t

val discontinue_with :
  backtrace:Printexc.raw_backtrace -> ('a, 'b) continuation -> exn -> 'b t
