(** {1:An introduction about Miou's internals.}

    This module proposes a re-interpretation of the effects management offered
    by OCaml 5 centered on the idea necessary in the implementation of a
    round-robin scheduler. For simplicity, as soon as we talk about scheduler,
    we will talk about round-robin scheduler. This one focuses on:
    - simplicity of understanding and ease of implementation
    - solving the starvation problem
    - the communist idea of being able to fairly share the CPU time to our tasks

    In this sense, the scheduler needs basic operations in the manipulation of
    tasks. Tasks are the functions (in the OCaml sense) to be executed. The
    scheduler must be able to:
    1) launch a task / apply a function
    2) stop a task at a specific point according to a metric
    3) have a representation of this suspended task (a {i state}) and be able to
       store it
    4) restart a task from its state

    Usually, the metric used to "stop" a task is time. That is to say that we
    could stop a task after 100ms has elapsed for example. Unfortunately, it
    seems difficult to translate this into OCaml. In this sense, the choice of
    our {i metric} is: {b effect}. That is to say that a task can only emit
    {i quanta} effects (which we will of course have to manage). The {i quanta}
    is the number of effects the task can produce.

    {3 Note about the [Effect] module.}

    If you follow us well and you are already familiar with the effects module
    in OCaml, you suspect that our implementation currently uses the
    {!module:Effect.Shallow} module which allows you to manage 1 effect (and
    only one).

    It is allowed to use the {!module:Effect.Deep} module for the implementation
    of a scheduler - the latter makes it possible to manage several effects
    until a {i certain} state of the function is obtained. In this case, when
    using the {!module:Effect.Deep} module, we spontaneously obtain 2 states:
    - a suspended state because we have obtained a blocking effect
    - or a task termination state.

    Note, however, the subtlety of the first in relation to what we want to
    "observe" of a task for our scheduler. This case explains the fact that the
    task stopped on a blocking event. We consider that this subtlety
    {i discriminates} the tasks (which is in opposition to our communist ideal)
    between those which block and those which do not block.

    In this sense, in our ideal and according to what is required by a
    round-robin scheduler, the suspension mechanism (stop a task) intervenes
    {b systematically} for each effect. Discrimination must be radically
    combated.

    {3 The goal of this interface.}

    This module therefore allows us to "direct" our development on these
    principles described above - indeed, the {!module:Effect} module is perhaps
    a little too permissive. Thus, if you have to modify this module and
    specifically this interface, beware of a "balkanization" effect which could
    betray our ideals.
*)

type ('a, 'b) continuation
(** The type of continuations. [('a, 'b) continuation] is the state of a
    function [_ -> 'b]. ['a] is the type of the value to {i continue}
    the continuation. The user can also {i discontinue} with an exception
    the continuation. *)

(** The type of function states.

    The state of a function is its execution state. A function can finish with
    its return value or an exception, or it can suspend on an
    {!type:'a Effect.t}. In the case of a suspension, the user can "continue"
    the execution via what is expected by the function depending on the effect
    and the function {!val:once}. Note that {!val:once} can only be used
    {b once} on a given value (otherwise, an exception
    {!exception:Continuation_already_resumed} is raised by OCaml). *)
type 'a t = private
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t

val make : ('a -> 'b) -> 'a -> 'b t
(** [make fn value] makes a new {i function state} by executing the function
    with the given argument. *)

(** Type of a step.

    A {i step} is how we want to continue (or not) a function state (see
    {!type:t}). The user {i continue} the execution of the given function
    state (see {!constructor:Send}), {i discontinue} it with an exception (see
    {!constructor:Fail}), do nothing (see {!constructor:Intr}) or replace the
    current effect by an another one (see {!constructor:Cont} as long as it
    expects the same type as the previous effect). *)
type 'a step =
  | Send of 'a
  | Fail of exn
  | Intr
  | Cont : 'a Effect.t -> 'a step
  | Yield : unit step

type ('a, 'b) k = ('a step -> 'b t) -> 'a Effect.t -> 'b t

type perform = { perform: 'a 'b. ('a, 'b) k } [@@unboxed]
(** Type of the effect handler.

    [perform] is a function which should handle incoming effects and give a
    {!type:step} to the given [k] according to the effect received. It's the
    {i effect handler}. *)

val once : perform:perform -> 'a t -> 'a t
(** [once ~perform state] applies [perform] once on the given state if the
    latter emits an effect. *)

val fail : exn:exn -> 'a t -> 'a t
(** [fail ~exn state] discontinue the given state with the given exception. It
    always return [Finished (Error exn)]. *)

val pure : ('a, exn) result -> 'a t
(** [pure value] returns [Finished value]. *)

val run : quanta:int -> perform:perform -> 'a t -> 'a t
(** [run ~quanta ~perform state] applies {!val:once} [quanta] times. If
    [perform] responds with {!constructor:Intr} (and therefore does nothing),
    even though there may be a few {i quanta} left, the function returns the
    last state obtained. *)

(** / **)

val continue_with : ('a, 'b) continuation -> 'a -> 'b t
val discontinue_with : ('a, 'b) continuation -> exn -> 'b t

(* I didn't sign for this. *)
