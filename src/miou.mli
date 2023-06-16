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
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concurrent : unit -> t
  val parallel : unit -> t
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
  (** [call fn] returns a promise which will be executed {b in parallel} with                                                  
      other promises. *)

  (* {2 Cancellation.} *)

  exception Cancelled

  val failed_with : 'a t -> exn -> unit

  val cancel : 'a t -> unit
  (** Cancellation allows the parent to kill a child using the associated                                                      
      promise. Cancellation marks the promise as {i consumed}, which means that                                                
      you can forget about the promise, and [miou] will not be informed that the                                               
      child is still alive. For instance, this is a valid code:                                                                

      {[                                                                                                                       
        # Miou.(run @@ fun () -> Prm.cancel (Prm.call (Fun.const 1))) ;;                                                       
        - : unit = ()                                                                                                          
      ]}                                                                                                                       

      We simply consider that cancellation is equivalent to looking into the                                                   
      promise (in other words, by using [cancel], you know that at worst, the                                                  
      state of the promise is [Failed Cancelled]). It may happen that you wish                                                 
      to cancel a promise that has already been resolved; in this case, no state                                               
      transition are made.                                                                                                     

      If you cancel a task, all the children in that task will be cancelled too.                                               
      Be careful, though, as cancelling them requires you to somehow observe the                                               
      transition of state of these children. If you follow [miou]'s rules, you                                                   
      should be looking after your children anyway.

      The cancellation mechanism does not actually destroy the task associated
      with the promise {b immediately}. In fact, the task can understand that it
      is being cancelled only if it interacts with [miou] (in other words, if it
      uses functions such as {!val:await} or {!val:yield}). For example, this
      code really consumes 1 second.

      {[
        # let sleep n = Unix.sleep 1 ;;
        # Miou.(run @@ fun () -> let a = Prm.call_cc (sleep 1) in
                yield ();
                Prm.cancel a) ;;
        - : unit = ()
      ]}

      The same applies to domains.

      Cancellation is quite strict on the management of promises, since the user
      explicitly cancels the promise, and is just as responsible for the
      "sub"-children: if the user cancels a promise, he/she must have a
      mechanism for properly waiting for or cancelling the children of this
      promise.

      {[
        # let () =
            Miou.run @@ fun () ->
            let a =
              Prm.call_cc @@ fun () ->
              let b = Prm.call_cc (Fun.const ()) in
              sleep 1.;
              Prm.await_exn b
            in
            yield (); Prm.cancel a ;;
        Exception: Miou.Still_has_childre.
      ]}


      For the example, this is a bad code because promise [b] always remains
      alive while at the same time cancelling its parent [a]. There must be a
      mechanism in which all the sub-promises must be cancelled before
      cancelling [a].

      The only case where such an approach is not necessary is in the
      {i abnormal} case: if the parent promise does not end normally (because of
      an exception). Here is an example of such a situation:

      {[
        # let () = Miou.run @@ fun () ->
            let p = Prm.call_cc @@ fun () ->
              let p' = Prm.call_cc (Fun.const ()) in
              if Random.bool ()
              then raise (Failure "p");
              Prm.await_exn p' in
            Prm.await_exn p
        Exception : Failure "p"
      ]} *)

  (** {2 Await a promise.} *)

  val await : 'a t -> ('a, exn) result
  val await_exn : 'a t -> 'a
  val await_first : 'a t list -> ('a, exn) result
  val await_first_exn : 'a t list -> 'a
  val await_all : 'a t list -> ('a, exn) result list
  val await_all_ign : 'a t list -> unit

  (** {2 Syscalls}

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

  val make : return:(unit -> 'a) -> 'a t
  (** [make ~return] creates a {i promise} that will {b never} be resolved. For
      the example, this code does not terminate:                                               

      {[                                                                                                                       
        # Miou.(run @@ fun () -> let v = Prm.make ~return:(Fun.const ()) in
                Prm.await v) ;;                                                      
      ]}                                                                                                                       

      However, if you keep this promise somewhere and specify an "events"                                                      
      function that proposes a task to resolve it, the program should                                                          
      then terminate:                                                                                                          

      {[                                                                                                                       
        # let global = ref None ;;                                                                                             
        # let events () = match !global with                                                                                   
          | Some prm -> Some [ Miou.syscall prm (Fun.const ()) ]                                                               
          | None -> None                                                                                                       
          ;;                                                                                                                   
        # Miou.(run ~events @@ fun () ->                                                                                       
          let v = Prm.make ~return:(Fun.const ()) in                                                                                              
          global := Some v; Prm.await v)                                                                                      
        - : (unit, exn) result = Ok ()                                                                                         
      ]}                                                                                                                       

      As you can see, the use of {!val:make} is very intrinsic to the creation                                                 
      of the [events] function. *)

  val uid : 'a t -> Id.t
  (** [uid syscall] returns a unique identifier of the promise. *)
end

val yield : unit -> unit
(** Suspends and schedules the current task, this gives other promises of the
    same {!type:Did.t} a chance to run, useful to be called in cpu-intensive
    paths. *)

type syscall

val syscall : 'a Prm.t -> (unit -> unit) -> syscall

val run :
     ?g:Random.State.t
  -> ?events:(unit -> syscall list option)
  -> (unit -> 'a)
  -> 'a
