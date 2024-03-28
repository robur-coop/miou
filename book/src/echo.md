# Implémenter un simple scheduler pour OCaml

The advantage of having a scheduler in OCaml is the ability to launch tasks
asynchronously, meaning to initiate tasks in the background. This enables
performing multiple tasks simultaneously without them potentially blocking each
other (in principle). Here, we will initially explore synchronous programming
with OCaml and examine its limitations. Subsequently, we will overcome these
limitations by implementing a small scheduler in OCaml.

## Synchronous programming

Consider the following code:
```ocaml
let name = "dinosaure"
let greeting = "Hello, my name is " ^ name ^ "!"
print_endline greeting
(* Hello, my name is dinosaure! *)
```

This code:
1) declares a string called `name`
2) declares another string called `greeting`, which uses `name`
3) outputs the `greeting` to your console

We should not here that your system effectively steps through the program one
line at a time, in the order we wrote it. At each point, the browser waits for
the line to finish its work before going on to the next line. It has to do this
because each line depends on the work done in the preceding lines.

That makes this a **synchronous program**. It would still be synchronous even if
we called a separate function like this:
```ocaml
let make_greeting name =
  "Hello, my name is " ^ name ^ "!"

let name = "dinosaure"
let greeting = make_greeting name
print_endline greeting
(* Hello, my name is dinosaure! *)
```

Here, `make_greeting` is a **synchronous function** because the caller has to
wait for the function to finish its work and return a value before the caller
can continue.

## A long-running synchronous function

What if the synchronous function takes a long time?

Some functions interact with your system and require resources. This is the
case, for example, with `input_line`, which actually requires user input. If you
run the code below, you'll notice that until you write a line, our message
doesn't appear.
```ocaml
let make_greeting name =
  "Hello, my name is " ^ name ^ "!"

let name = "dinosaure"
let greeting = make_greeting name
ignore (input_line stdin);
print_endline greeting  
```

We say that the `input_line` function **blocks**. And we can't do anything else
as long as this function is blocked. This type of function make it impossible
for a program to carry on other computations while it is waiting for the latter
to finish. For some programs, that's just fine. A text adventure game, for
example, doesn't have any _background_ computations it needs to perform. But
other programs, like spreadsheets or servers, would be improved by being able to
carry on computations in the _background_ rather than having to completely block
while waiting for input.

The reason for this is that OCaml program is _single-threaded_[^multicore]. A
thread is a sequence of instructions that a program follows. Because the program
consists of a single thread, it can only do one thing at a time: so if it is
waiting for our long-running synchronous call to return, it can't do anything
else.

What we need is a way for our program to:
- Be able to run a blocking/long-running function in the _background_ as a task
- Obtain a _state_ for this task to see its status
- Be notified of the end of our task using our _state_

That's precisely what asynchronous functions enable us to do. 

[^multicore]: The more curious will say that OCaml is "multicore", and that's
true. However, it is if you want to use the `Domain`/`Thread` module and
allocate a domain/thread that can do a task in parallel or concurrently. But
we'll explain all this in detail later.
