module Tq : sig
  (** A lock-free queue.                                                                                                       

      To be able to implement a scheduler across multiple domains, we must                                                     
      have a Thread-safe Queue. This thread-safe implementation provides basic                                                 
      operations for a queue: {!val:enqueue} & {!val:dequeue}. *)

  type 'a t
  (** Type of lock-free queues. *)

  exception Empty

  val enqueue : 'a t -> 'a -> unit
  val dequeue : 'a t -> 'a
  val make : unit -> 'a t
  val is_empty : 'a t -> bool
end

module Did : sig
  (** An unique identifier for domains. *)

  type t
  (** The type of identifiers. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  val self : unit -> t
  (** [self ()] returns the identifier of the current domain that runs you. *)
end

module Id : sig
  (** A unique identifier for promises. *)

  type t
  (** The type of identifiers. *)

  val null : t
  (** [null] is an {i impossible} value of {!type:t}. Actually, {!type:t} is                                                   
      used to identify {!type:Prm.t} and they will {b never} have such                                                         
      value as their identifiers. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Prm : sig
  (** A promise for a better futur.                                                                                            

      A {b promise} is a function ([unit -> 'a]) that will be executed by the                                                  
      scheduler in the near future. The user can launch a promise (and notify                                                  
      by this way the scheduler of a new task to do) as well as wait for the                                                   
      result of a promise (see {!val:await}).                                                                                  

      A promise can be executed concurrently with other promises (see                                                          
      {!val:call_cc}) or in parallel with other promises (see {!val:call}).                                                    
  *)

  type 'a t
  (** Type of promises. *)

  val pp : Format.formatter -> 'a t -> unit
  (** A simple pretty-printer of a promise which shows you the domain where                                                    
      the promise run and its unique ID. *)

  (** {2 Launch a promise.} *)

  val call_cc : (unit -> 'a) -> 'a t
  (** [call_cc fn] (for Call with Current Continuation) returns a promise which                                                
      will be executed {b cooperatively} with other promises. *)

  val call : (unit -> 'a) -> 'a t
  (** [call ?pool fn] returns a promise which will be executed {b in parallel}
      with other promises. [miou] pre-allocates domains that are waiting to
      perform this kind of task. The user does {b not} choose the domain on
      which the task will be executed. [miou] {b randomly} chooses which of the
      domains will perform the task. *)

  (* {2 Cancellation.} *)

  exception Cancelled

  val cancel : 'a t -> unit
  (** Cancellation allows the parent to kill a child using the associated
      promise. Cancellation marks the promise as {i consumed}, which means that
      you can forget about the promise, and [miou] will not be informed that the
      child is still alive. For instance, this is a valid code:

      {[
        # Miou.(run @@ fun () -> Prm.cancel (Prm.call (Fun.const ()))) ;;
        - : unit = ()
      ]}

      We simply consider that cancellation is equivalent to looking into the
      promise's result (in other words, by using [cancel], you know that at
      worst, the promise's result is [Failed Cancelled]). It may happen that you
      wish to cancel a promise that has already been resolved; in this case,
      {b no} state transition are made.                                                                                                     

      {2 Schrödinger's cat.}

      Cancelling a parallel task can be difficult. Even if there are internal
      mechanisms for synchronising between domains, this only results in a state
      that is synchronous with the state of the promise. And this state can be
      {i resolved}... So, this code has 2 results:

      {[
        # let prgm () = Miou.(run @@ fun () ->
          let p = Prm.call (Fun.const ()) in
          Prm.cancel p; Prm.await p) ;;
        # prgm () ;;
        - : (unit, exn) result = Ok ()
        # prgm () ;;
        - : (unit, exn) result = Error Prm.Cancelled
      ]}

      This example also shows that if the domain solves our task faster than
      our other domain tries to cancel it, we keep the solved result. As we
      said, there is no state transition if the promise has already been
      resolved. *)

  (** {2 Await a promise.} *)

  val await : 'a t -> ('a, exn) result
  val await_exn : 'a t -> 'a
  val await_one : 'a t list -> ('a, exn) result
  val await_all : 'a t list -> ('a, exn) result list
  val await_first : 'a t list -> ('a, exn) result

  (** {2 Syscalls.}

      [miou] does not interact with the system, only with the OCaml runtime. As                                                
      a result, it does not implement the usual input/output operations.                                                       
      Nevertheless, it offers a fairly simple API for using functions that                                                     
      interact with the system (and that can, above all, block).                                                               

      One of the rules of [miou] is never to give it blocking functions to eat                                                 
      (in fact, it has very strict - but very simple - nutritional constraints).                                               

      On the other hand, the system can inform you when a function is                                                          
      non-blocking (and can therefore be given to [miou]). The idea is to inform                                               
      [miou] of the existence of a {i promise}, which it will then try to                                                      
      resolve. Of course, it won't be able to, but as a last resort, [miou] will                                               
      come back to you to ask for a possible task to resolve this promise. It                                                  
      will do this via an user's defined function, which you can specify using                                                 
      the {!val:run} function (see [events] argument).                                                                         

      This user's defined function return a {!type:syscall} which is a promise                                                 
      associated with a {b non-blocking} task ([unit -> unit]) that would
      resolve it. At last, [miou] will be able to fulfil your promise!                                                                 

      For more information on this API, a tutorial is available on how to                                                      
      implement {!page:sleepers}: tasks that block your process for a time.                                                        
   *)

  type 'a syscall

  val make : return:(unit -> 'a) -> 'a syscall
  (** [make ~return] creates a {i promise} that will {b never} be resolved. For
      the example, this code fails:

      {[
        # Miou.(run @@ fun () -> let v = Prm.make ~return:(Fun.const ()) in
                Prm.suspend v) ;;
        Exception: Miou.Still_has_children
      ]}

      However, if you keep this promise somewhere and specify a "select" 
      function that proposes a task to resolve it, the program should then
      terminate:

      {[
        # let global = ref None ;;
        # let select () = match !global with
          | Some p -> [ Miou.task p (fun () -> global := None) ]
          | None -> []
          ;;
        # let events = { Miou.select; Miou.interrupt= ignore } ;;
        # Miou.(run ~events @@ fun () ->
          let v = Prm.make ~return:(Fun.const ()) in
          global := Some v; Prm.suspend v) ;;
        - : (unit, exn) result = Ok ()
      ]}

      As you can see, the use of {!val:make} is very intrinsic to the creation
      of the [events] function. *)

  val suspend : 'a syscall -> ('a, exn) result
  (** [suspend syscall] suspends the execution flow and will be resumed when the
      user gives a {b non-blocking} function (a {!type:task}) via {!type:events}
      that resolves the syscall. *)

  val is_pending : 'a syscall -> bool

  val uid : 'a syscall -> Id.t
  (** [uid syscall] returns a unique identifier of the promise. *)
end

val yield : unit -> unit
(** [yield ()] reschedules tasks and give a chance to all of them to be executed
    then. For instance:

    {[
      # Miou.run @@ fun () ->
        let p = Prm.call_cc (fun () -> print_endline "Hello") in
        print_endline "World";
        Prm.await_exn prm ;;
      World
      Hello
      - : unit = ()
      # Miou.run @@ fun () ->
        let p = Prm.call_cc (fun () -> print_endline "Hello") in
        yield ();
        print_endline "World";
        Prm.await_exn prm ;;
      Hello
      World
      - : unit = ()
    ]} *)

type task
type events = { interrupt: unit -> unit; select: unit -> task list }

val task : 'a Prm.syscall -> (unit -> unit) -> task

val run :
     ?g:Random.State.t
  -> ?domains:int
  -> ?events:(unit -> events)
  -> (unit -> 'a)
  -> 'a
