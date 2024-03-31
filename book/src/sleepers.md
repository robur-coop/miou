# Interacting with the system with Miou

Miou is structured similarly to our small scheduler, but with a clear
distinction between scheduling and system interaction.

The goal is to be able to inject a certain type of interaction based on the
system used, which may not necessarily be `Unix`. As mentioned in the
introduction, our cooperative also aims to implement services as unikernels.
These are very specific systems where the concept of files may not even exist.
In this sense, `Miou_unix` is specific to `Unix`.

`Unix` and `Miou_unix` may suffice for most use cases. However, they may not be
suitable for unikernels and may not meet your criteria and system requirements.
A system like Linux may offer alternative means such as `epoll()` for
interacting with the external world, for example.

Beyond other possibilities, subtle differences may also exist. Indeed, systems
like Linux or \*BSD may not necessarily offer the same semantics in their
handling of TCP/IP connections, for instance. These nuances make it difficult
and even _error-prone_ to try to standardize all these interactions. Overall, we
believe that you are the best person to know how to interact with the system. As
such, we offer this small tutorial to guide you on how to implement the _glue_
between Miou and your system.

## Sleepers

One of the simplest interactions we can implement with Miou is the _sleeper_.
It's akin to what `Unix.sleep` does: aiting for a certain amount of time. We'll
iterate through its implementation to:
- Start by offering such a function.
- Handle the scenario where multiple domains might use this function.
- Finally, manage the cancellation of tasks that have called this function.

To guide our tutorial, here's the code we aim to execute. The module `Chat` is
the one we need to implement.
```ocaml
let program () =
  Chat.run @@ fun () ->
  let a = Miou.call_cc @@ fun () -> Chat.sleep 1. in
  let b = Miou.call_cc @@ fun () -> Chat.sleep 2. in
  Miou.await_all [ a; b ]
  |> List.iter @@ function
  | Ok () -> ()
  | Error exn -> raise exn

let () =
  let t0 = Unix.gettimeofday () in
  program ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)
```

If we assume that tasks run "simultaneously," this program should execute in
less than 3 seconds.

## Our sleeper function

If you remember from our little scheduler, we reused our `Await` effect to
implement our `our_accept` function. We then injected our `our_select` into our
scheduler so that it could observe events and signal suspension points to resume
them.

Similarly, Miou offers such mechanisms. You can suspend a function using a
unique identifier called `Miou.syscall`:
```ocaml
type syscall
type uid = private int [@@immediate]

val syscall : unit -> syscall
val uid : syscall -> uid
val suspend : syscall -> unit
```

The equivalent of our `Effect.perform (Await prm)` from our little scheduler
would be `Miou.suspend` syscall. Therefore, our main goal is to save our syscall
_somewhere_ and attach the time we need to wait to it:
```ocaml
type sleeper =
  { time : float
  ; syscall : Miou.syscall }

module Sleepers = Miou.Pqueue.Make (struct
  type t = sleeper

  let compare { time= a; _ } { time= b; _ } = Float.compare a b
  let dummy = { time= 0.0; syscall= Obj.magic () }
end)

let sleepers = Sleepers.create ()

let sleep delay =
  let time = Unix.gettimeofday () +. delay in
  let syscall = Miou.syscall () in
  Sleepers.insert { time; syscall } sleepers;
  Miou.suspend syscall
```

Using a priority queue will allow us to get the syscall that needs to resume its
function as soon as possible among all our sleepers.

Now we need to find a way to inject a function that will be called periodically,
in which we can resume our functions just like we did for our little scheduler
with our `our_select` function. Fortunately, Miou offers this possibility of
injecting such a function via `Miou.run`:
```ocaml
type select = block:bool -> uid list -> signal list
type events = { select: select; interrupt: unit -> unit }

val run : ?events:(Domain.Uid.t -> events) -> (unit -> 'a) -> 'a
```

Au tout début, Miou tente d'allouer les domaines. Pour chaque domaine, il va
demander une valeur `event` qui contient une fonction `select`. C'est cette
fonction qu'on doit implémenter et proposer à Miou. Cette fonction sera appellé
dès l'existence d'un syscall à chaque fois qu'on aura exécuté une partie de nos
tâches et de manière périodique. Cette fonction dois retourner ce qu'on nomme
des signaux permettant ensuite à Miou de relancer les fonctions qui ont été
suspendu par nos syscalls.

At the very beginning, Miou tries to allocate the domains. For each domain, it
will ask for an `event` value that contains a `select` function. This is the
function we need to implement and provide to Miou. This function will be called
whenever there is a syscall after executing a portion of our tasks and
periodically. This function must return signals allowing Miou to resume
functions that have been suspended by our syscalls.
```ocaml
type signal

val signal : syscall -> signal
```

So, one of its goals is to determine if one of our sleepers should resume one of
our tasks or not.
```ocaml
let in_the_past ts = ts = 0. || ts <= Unix.gettimeofday ()

let rec remove_and_signal_older sleepers acc =
  match Sleepers.find_min sleepers with
  | None -> acc
  | Some { time; syscall } when in_the_past time ->
    Sleepers.delete_min_exn sleepers;
    remove_and_signal_older sleepers (Miou.signal syscall :: acc)
  | Some _ -> acc

let select ~block:_ _ =
  let ts = match Sleepers.find_min sleepers with
    | None -> 0.0
    | Some { time; _ } ->
      let value = time -. Unix.gettimeofday () in
      Float.max 0.0 value in
  Unix.sleepf ts;
  remove_and_signal_older sleepers []

let event _ = { select; interrupt= ignore }
let run fn = Miou.run ~events fn
```

Comme on peut le constater, on fait une spécialisation de notre fonction `Miou.run`
avec notre `events`. C'est pour cette raison que dès que vous utiliserez des fonctions
de `Miou_unix` par exemple, vous devez utiliser `Miou_unix.run`. Essayons désormais notre
code:
```shell
$ ocamlfind opt -linkpkg -package miou,unix -c chat.ml
$ ocamlfind opt -linkpkg -package miou,unix chat.cmx main.ml
$ ./a.out
$ echo $?
0
```

Et voilà! On vient de prouver que nos deux tâches s'exécutent en même temps. On notera
l'utilisation de `Unix.sleepf` au lieu de `Unix.select`. On ne s'intéresse qu'à attendre
ici plutôt qu'à observer nos file-descriptors.

## Domains & system

Comme il a été mentionné, Miou gère plusieurs domaines. Ainsi, si notre `select` utilise
une variable global (tel que `sleepers`), nous aurons définitivement un problème d'accès
à cette variable par les domaines. Il existe plusieurs solution dont une consiste à
"protéger" notre global à l'aide d'une Mutex. Cependant, nous avons précisé que chaque
domaine gèrent son `select`.

Plus généralement, un `syscall` est toujours local à un domaine, il ne peut être géré
par une autre domaine qui ne l'a pas suspendu. En ce sens, on peut considérer allouer
un `sleepers` par domaines. Il existe un moyen en OCaml de considérer des valeurs
qui existent et sont accessible pour chaque domaine, et ces derniers ont l'exclusivité
dessus: c'est le [Thread Local Storage][tls].

Ainsi, au lieu d'avoir une global pour notre `sleepers`, nous allons utiliser cette API:
```ocaml
let sleepers =
  let key = Stdlib.Domain.DLS.new_key Sleepers.create in
  fun () -> Stdlib.Domain.DLS.get key

let sleep delay =
  let sleepers = sleepers () in
  let time = Unix.gettimeofday () +. delay in
  let syscall = Miou.syscall () in
  Sleepers.insert { time; syscall } sleepers;
  Miou.suspend syscall

let select ~block:_ _ =
  let sleepers = sleepers () in
  let ts = match Sleepers.find_min sleepers with
    | None -> 0.0
    | Some { time; _ } ->
      let value = time -. Unix.gettimeofday () in
      Float.max 0.0 value in
  Unix.sleepf ts;
  remove_and_signal_older sleepers []
```

On peut désormais remplacer nos `Miou.call_cc` par des `Miou.call` en toute
sécurité. On sait que chacune des tâches aura son propre `sleepers` et qu'il
n'y aura pas d'accès illégaux entre les domaines.
