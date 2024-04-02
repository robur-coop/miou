# Conditions & Mutexes

Lorsqu'il était question d'implémenter notre petit scheduler et d'intéragir avec
le système, il était essentiellement question de résoudre la question de la suspension
d'une fonction qui puisse se mettre en tâche de fond. Cependant, il n'y a pas que
les syscalls qui peuvent suspendre/bloquer l'exécution d'une fonction. Il y a aussi
les Mutexes et les Conditions.

Le réel défi d'un scheduler est de pouvoir suspendre des fonctions sans passer
par le système: en un mot, gérer **toutes** les suspensions. Pour les novices,
les Mutexes et les Conditions permettent de bloquer et débloquer l'exécution
d'une fonction (et ceci, peut être sous la _condition_ d'un prédicat).

L'intérêt de tels mécanismes permet de _synchroniser_ des tâches entre elles. Qu'elles soient
en concurrence et/ou en parallèle, il est difficile, voir impossible de savoir qu'elle tâche
va s'exécuter avant les autres. Cependant, nous voudrions parfois (et même souvent), partager
des informations entre ces tâches. Miou n'autorise qu'un type de transfert d'informations
entre les tâches: des enfants à leurs parents direct.

Dans tout les autres cas (par exemple, en deux tâches n'ayant pas d'affiliation et
s'exécutant en parallèle), il nous faut réfléchir à comment transférer ses informations
et de manière correct (c'est à dire que ce transfert fonctionnerait qu'importe l'ordre
d'exécution de nos tâches du point de vue de Miou - pour Miou.call_cc - mais aussi du système - pour Miou.call).
C'est dans ces cas là que les mutexes et les conditions peuvent être utiles.

## Mutexes

Les mutexes permettent d'obtenir l'exclusivité de manipuler des informations par
rapport aux autres domaines. C'est à dire que l'on peut manipuler une ressource 
global et disponible auprès de tout les domaines de manière sécurisé à l'aide des
mutexes. Pour illustrer notre exemple, nous allons reprendre notre serveur `echo` où nous
voudrions afficher (comme des logs) les connexions entrantes:
```ocaml
let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX v -> Format.pp_print_string ppf v
  | Unix.ADDR_INET (inet_addr, port) ->
    Format.fprintf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr, port)

let server () =
  let socket = Miou_unix.Ownership.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Miou_unix.Ownership.bind_and_listen socket sockaddr;
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, sockaddr = Miou_unix.Ownership.accept socket in
    Format.printf "new client: %a\n%!" pp_sockaddr sockaddr;
    ignore (Miou.call_cc
      ~give:[ Miou_unix.Ownership.resource clientr 
      ~orphans (fun () -> echo client))
  done;
  Miou_unix.Ownership.close socket
```
