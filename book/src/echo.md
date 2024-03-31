# An echo server with Miou

Passer de notre petit scheduler à Miou est assez simple. Mise à part la règle
de ne pas oublier ses enfants que nous avons introduit dans le chapitre précédent,
il s'agit principalement d'utiliser `Miou` et `Miou_unix`. Prennons la fonction
`echo` qui gère nos clients:

Moving from our custom scheduler to Miou is quite straightforward. Apart from
the rule of not forgetting our children that we introduced in the previous
chapter, it mainly involves using `Miou` and `Miou_unix`. Let's take the `echo`
function that handles our clients:
```ocaml
let rec echo client =
  let buf = Bytes.create 0x100 in
  let len = Miou_unix.read client buf 0 (Bytes.length buf) in
  if len = 0 then Miou_unix.close client
  else
    let str = Bytes.sub_string buf 0 len in
    let _ = Miou_unix.write client str 0 len in echo client
```

A subtlety lies in `Miou_unix.write`, which expects a `string` instead of
`bytes`. In a concurrent execution, it may happen that `buf` could be modified
concurrently. Using a `string` ensures that the buffer we want to transmit does
not change in the meantime. Once again, this comes from our experience in
protocol implementation.

Then, according to the rules introduced in the previous chapter, we need to
re-implement our server so that it uses an `orphans` value and periodically
_cleans up_ our terminated clients to avoid forgetting our children:
```ocaml
let clean_up orphans = match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> match Miou.await prm with
    | Ok () -> ()
    | Error exn -> raise exn

let server () =
  let socket = Miou_unix.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Miou_unix.bind_and_listen socket sockaddr;
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, _ = Miou_unix.accept socket in
    ignore (Miou.call_cc ~orphans (fun () -> echo client))
  done;
  Miou_unix.close socket

let () = Miou_unix.run server
```

Note the use of `Miou_unix.run` to handle system events and the functions from
this module. For more details, we will explain the interactions between the
system and Miou more precisely in the next chapter.

And there we go, we've switched to Miou! All we need to do is compile our
project like this:
```shell
$ opam install miou
$ ocamlfind opt -linkpkg -package miou,miou.unix main.ml
$ ./a.out &
[1] 436169
$ echo "Hello" | netcat -q0 localhost 3000
Hello
$ kill -9 436169
[1]  + 436169 killed     ./a.out
```

## Parallelism

Puisqu'il est possible d'utiliser désormais plusieurs domaines, prennons cette
avantage pour ne pas instancier qu'un seul serveur. En effet, il est possible
à ce que plusieurs serveur existent sur une même addresse (`localhost:3000`).
Dans une tel situation, c'est le premier prêt qui est le premier servi. On peut
donc imaginer gérer en parallèle plusieurs serveur qui géreront, en concurrence
cette fois ci, plusieurs clients.

Since it's now possible to utilise multiple domains, let's take advantage of
this to instantiate more than one server. Indeed, it's conceivable that multiple
servers could exist at the same address (`localhost:3000`). In such a scenario,
it's first come, first served. Therefore, we can envision managing multiple
servers in parallel, each handling several clients concurrently.

To distribute the implementation of our server across multiple domains, we'll
use `Miou.parallel`. We won't forget to involve `dom0` (referring to our rule
where `dom0` would never be assigned a task from other domain) via
`Miou.call_cc`:
```ocaml
let () = Miou_unix.run @@ fun () ->
  let domains = Stdlib.Domain.recommended_domain_count - 1 in
  let domains = List.init domains (Fun.const ()) in
  let prm = Miou.call_cc server in
  Miou.await prm :: Miou.parallel server domains
  |> List.iter @@ function
  | Ok () -> ()
  | Error exn -> raise exn
```

To ensure that we're leveraging the full potential of our machine, we can check
how many threads our program has (note that an OCaml domain always has 2
threads!). Thus, for a system with 32 cores:
```shell
$ ocamlfind opt -linkpkg -package miou,miou.unix main.ml
$ ./a.out &
[1] 438053
$ ls /proc/438053/task | wc -l
64
$ kill -9 438053
[1]  + 438053 killed     ./a.out
```

Almost for free, we've managed to launch multiple servers in parallel!

## Ownership

When it comes to building system and network applications, we often deal with
resources shared between the application and the system. One such resource we
use here is the file descriptor. OCaml has the advantage of offering a garbage
collector to handle memory management for us. However, we still need to consider
releasing system resources, particularly file descriptors.

Another point to consider is the manipulation of these resources. We subtly
mentioned, using `Miou_unix.write`, the possibility that a buffer could be
concurrently modified. From our experience, the concept of resource ownership
(like a buffer) specific to a particular task is lacking in OCaml and can lead
to rather challenging bugs to identify and understand. In this regard, languages
like Rust offer solutions that can help developers avoid a resource being
manipulated by two tasks "at the same time". The problem is even more
significant with tasks that can run in parallel. This is referred to as a
[data race][data-race].

Therefore, the best we can offer, Miou provides resource management that
resembles that of Rust: a task has exclusive access to a resource once it has
"proof" of ownership.

Miou offers an API, `Miou.Ownership`, where you can:
- Create proof of ownership
- Own a resource through this proof
- Disown a resource through this proof
- Transfer this resource to a child via this proof
- Transfer this resource to the parent via this proof
- Verify, before manipulating this resource, that you have exclusive access to
  it

`Miou_unix` extends this API to file descriptors. In this second part of this
chapter, it's essential to ensure that we are indeed the owners of the file
descriptor we are manipulating. This won't change the behavior of our server; it
just allows us to sleep better tonight!

Let's start with the `echo` function:
```ocaml
let rec echo client =
  let buf = Bytes.create 0x100 in
  let len = Miou_unix.Ownership.read client buf 0 (Bytes.length buf) in
  if len = 0 then Miou_unix.Ownership.close client
  else
    let str = Bytes.sub_string buf 0 len in
    let _ = Miou_unix.Ownership.write client str 0 len in echo client
```

`Miou_unix.Ownership.{read,write}` perform the necessary checks, while
`Miou_unix.Ownership.close` disown our file descriptor since we no longer need
it. Forgetting this step would result in an error in your application, and Miou
would notify you that a resource has been forgotten (via
`Miou.Resource_leaked`). Once attached to a task, a resource must be transferred
or released; otherwise, it's considered forgotten! The aim is truly to assist
the developer in not forgetting anything they manipulate.

Another interesting aspect of resources is the case of an abnormal termination
of our `echo` function via an exception. A resource is also associated with a
_finalizer_ that will be executed if the task in possession of the resource
terminates abnormally. Again, the goal is to sleep well tonight.

Now, let's move on to our `server` function, where we need to transfer our
client file descriptor to our `echo` task:
```ocaml
let server () =
  let socket = Miou_unix.Ownership.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Miou_unix.Ownership.bind_and_listen socket sockaddr;
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, _ = Miou_unix.Ownership.accept socket in
    ignore (Miou.call_cc
      ~give:[ Miou_unix.Ownership.resource clientr 
      ~orphans (fun () -> echo client))
  done;
  Miou_unix.Ownership.close socket
```

And there you have it! Assuming everything goes well, our code is correct, and
we are using our resources correctly. The `Miou.Ownership` module is not
mandatory in Miou's usage but provides a value to dynamically verify the proper
use and transfer of your resources. While it's not obligatory, we strongly
recommend using it.

The _finalizer_ associated with the resource can also be genuinely beneficial,
especially when cancellation occurs: it ensures there are no leaks, even in
abnormal situations.

## Conclusion

If you've made it this far, you've likely seen a good portion of what Miou has
to offer and delved into the intricacies of asynchronous programming and system
interactions. You can continue experimenting and having fun with Miou or delve
deeper into our tutorial.

If you recall our initial challenge with our `echo` server, we divided the
subject into two parts: the scheduler and system interactions. Miou also
maintains this separation between the `Miou` module and the `Miou_unix` module.
The next chapter will revisit system interactions, but this time with Miou. The
goal will be to implement _sleepers_ (and replicate `Unix.sleep`).

Finally, the last chapter is an enhancement of our `echo` server using Mutexes
and Conditions provided by Miou. This chapter explains in detail the benefits of
using these modules over those offered by OCaml and presents, once again, a
concrete case.
