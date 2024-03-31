# A simple scheduler

In our previous chapter, we discussed the idea of backgrounding our `echo`
function to continue accepting incoming connections. Let's stick with the idea
of being able to background functions:
```ocaml
let fn () = print_endline "World"

let () =
  do_this_in_background fn;
  print_endline "Hello"
```

Here, `do_this_in_background` would be a function that takes a task and adds it
to a kind of hidden to-do list. Instead of being literally synchronous and
waiting to display "World" before displaying "Hello", we could imagine directly
displaying "Hello" and letting _something_ execute our `fn` function to display
"World".

This "something" is what we call a **scheduler**. It holds our list of tasks to
do and attempts to execute them all in a specific order. In our example, we
would notify our scheduler that a task `fn` needs to be done, display "Hello",
and then _wait_ for our scheduler to complete all tasks (including displaying
"World").

In our explanation, we subtly introduced a new concept: waiting. In reality,
what we want is to wait for all our tasks to finish. To do this, we could have a
"witness" for our tasks so that we can wait for these tasks through their
witnesses:
```ocaml
let fn () = print_endline "World"

let () =
  let witness = do_this_in_background fn in
  print_endline "Hello";
  await witness
```

We're starting to get closer to what every scheduler aims to provide and what
asynchronous programming is all about. Let's keep this example in mind and move
on to another concept necessary for implementing our scheduler.

## Effects

Since OCaml 5, it has been possible to utilise effects. An effect allows you to
pause the execution of a function and enter a handler, which, depending on the
effect, would execute a specific operation to resume the paused function with
the result of that operation.

Here, we're delving into the flow of execution in your program. If we revisit
our definition of synchronicity, we understand that our system processes the
program line by line. However, effects (as well as exceptions) break this flow;
they're known for breaking the linear progression of execution. Consider the
example of an exception:
```ocaml
exception My_exception

let () =
  try print_endline "Hello";
      raise My_exception;
      print_endline "World"
  with My_exception -> print_endline "My_exception"
```

In this scenario, our code will print "Hello" and then trigger an exception.
Consequently, the subsequent line won't execute. Instead, this exception will be
"caught" by our handler `with ...`. This mechanism attempts to identify the
raised exception and, based on that, execute certain code — in our example,
printing "My_exception".

Raising exceptions or triggering an effect can be likened to a jump in our code.
The key distinction between an exception and an effect lies in the ability, for
the latter, to return to the point where the effect was initiated.
```ocaml
open Effect.Deep

type _ Effect.t += My_effect : unit Effect.t

let run fn v =
  let rec retc x = x
  and exnc = raise
  and effc
    : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option
    = function
    | My_effect ->
      print_endline "My_effect";
      Some (fun k -> continue k ())
    | _ -> None
  and handler = { retc; exnc; effc; } in
  match_with fn v handler

let my_program () =
  print_endline "Hello";
  Effect.perform My_effect;
  print_endline "World"

let () = run my_program ()
```

The mechanics are a bit more intricate, but the principle remains consistent.
Upon executing the above code, we'll observe that we indeed enter our handler
(and display "My_effect" akin to using a `with ...` block for exceptions), but
we return to the precise point where the effect was initiated and then proceed
to display "World".

When considering our core challenge of implementing a scheduler, the utility of
effects becomes apparent in obtaining the value `k` as a representation of our
suspended function at a specific point — where we triggered our effect. For
managing tasks, with each task as a function, this `k` allows us to suspend and
resume functions, maintaining them in a suspended state in the background.

Indeed, for our scheduler, maintaining this suspension is crucial. Rather than
simply performing an operation and continuing with the result, we aim to:
1) keep the suspension in background
2) allow other tasks to execute.
3) resume the suspension after giving the opportunity for other tasks to
   execute.

## A task

Now, we need to define what a task is. Earlier, we mentioned this value `k`,
which would represent a suspended state of our function. We could define what a
task is (an OCaml function) and in what state this task is:
- an initial state
- a suspended state that can be resumed
- a termination state

```ocaml
open Effect.Shallow

type 'a state =
  | Initial of (unit -> 'a)
  | Suspended : ('c, 'a) continuation * 'c Effect.t -> 'a state
  | Resolved of 'a
```

Just like with exceptions, we need to "attach" a handler to catch effects and
obtain that famous `k`. We can envision a generic handler that generates this
state from the effects produced by a function:
```ocaml
let handler =
  let open Effect.Shallow in
  let retc v = Resolved v in
  let exnc = raise in
  let effc
    : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option
    = fun eff -> Some (fun k -> Suspended (k, eff)) in
  { retc; exnc; effc }
```

Finally, we can define a task simply as our state.
```ocaml
type task = Task : 'a state -> task
```

At this stage, we have tasks, and it is now essential to define the operations
that we can perform with them. If we revisit our code from the very beginning,
which encapsulates the idea that we want to put tasks in the background, we
essentially perform two operations:
- we notify the scheduler of a new task (with our `do_this_in_background`).
- we wait for our task with a witness of it.

## A promise

This witness holds a term commonly found in asynchronous programming: a
**promise**. Indeed, this witness is a _promise_ that our task will be executed
(but is not executed yet). Waiting for a task via its promise simply corresponds
to obtaining the result of our task. In our example, this result is `() : unit`
because we are merely displaying text, but we could easily imagine a hefty
computation (such as finding a prime number) that we would want to run in the
background.

In short, the promise would allow us to obtain this result. We could define it
as a mutable value that changes as soon as our task is completed:
```ocaml
type 'a promise = 'a option ref
```

Now we can define our effects that will interact with our scheduler:
```ocaml
type _ Effect.t += Spawn : (unit -> 'a) -> 'a promise Effect.t
type _ Effect.t += Await : 'a promise -> 'a Effect.t
```

## Our scheduler

Our types are defined, and we know how to obtain them. Now, all we need to do is
implement our scheduler. As mentioned, the scheduler simply maintains a list of
tasks to be done. Therefore, it's a `task list` that we'll be manipulating. The
action of `Spawn` will enlarge this list, while `Await` will observe the state
of our promise, which will change as soon as its associated task is completed.
Waiting can result in two different situations:
- the case where, indeed, the promise has been resolved. In this case, we simply
  transmit its result.
- the case where it is not yet resolved. In this particular situation, we give
  the opportunity for other tasks to execute (which can help in resolving our
  initial task). This is referred to as _yielding_.
```ocaml
let perform
  : type c. task list ref -> c Effect.t -> [ `Continue of c | `Yield ]
  = fun todo -> function
  | Spawn fn ->
    let value = ref None in
    let task = Initial (fun () -> value := Some (fn ())) in
    todo := !todo @ [ Task task ] ;
    `Continue value
  | Await value ->
    begin match !value with
    | Some value -> `Continue value
    | None -> `Yield end
  | _ -> invalid_arg "Invalid effect"
```

Finally, it's just a matter of iterating over this list to gradually complete
all our tasks. This iteration involves observing the state of each of our tasks.
- For the initial state, we simply launch the task and see what we obtain
  through the handler we defined earlier (i.e., whether the task is resolved or
  suspended).
- For the resolved state, there's nothing to do; our task has finished.
- Lastly, for the suspended state, we need to determine what operation our
  effect produces (using `perform`). The case of yielding is interesting because
  it involves keeping our suspension in our to-do list and attempting to execute
  our other tasks first.

```ocaml
let step todo = function
  | Initial fn ->
    Effect.Shallow.(continue_with (fiber fn) () handler)
  | Resolved v -> Resolved v
  | Suspended (k, effect) ->
    match perform todo effect with
    | `Continue v -> Effect.Shallow.(continue_with k v handler)
    | `Yield -> Suspended (k, effect)

let run fn =
  let result = ref None in
  let rec go = function
    | [] -> Option.get !result
    | Task task :: rest ->
      let todo = ref rest in
      match step todo task with
      | Resolved _ -> go !todo
      | (Initial _ | Suspended _) as task -> go (!todo @ [ Task task ]) in
  let task = Initial (fun () -> result := Some (fn ())) in
  go [ Task task ]
```

## Let's play!

Have you kept in mind our initial code and our primary goal? It was about
putting a task in the background and executing it afterward. Returning to our
basic problem, we wanted to manage our clients as background tasks while
effectively handling the reception of new connections. Let's revisit the
original code:
```ocaml
let fn () = print_endline "World"

let () =
  let witness = do_this_in_background fn in
  print_endline "Hello";
  await witness
```

With our scheduler, this code would become:
```ocaml
let spawn fn = Effect.perform (Spawn fn)
let await prm = Effect.perform (Await prm)

let fn () = print_endline "World"

let () = run @@ fun () ->
  let prm = spawn fn in
  print_endline "Hello";
  await prm
```

If we compile all of this and run the code, we get:
```shell
$ ocamlopt main.ml
$ ./a.out
Hello
World
```

Et voilà! Our task displaying "World" was successfully put into the background,
and we indeed displayed "Hello" first. We now have the basics of our scheduler.
You now understand the core concepts of all schedulers (whether in OCaml or
JavaScript). Several (perhaps suboptimal) choices were made, but the most
important thing is to grasp the concept of asynchronous programming through a
concrete example.

Now, it's time to address our initial problem: managing our clients while also
accepting new connections. At this stage, you might think that simply "spawning"
our `echo` function will make it work in the background. However, even though
we've addressed the issue of synchronicity by offering an asynchronous library,
we deliberately overlooked mentioning **blocking** functions! That's what we'll
explore in our next chapter.
