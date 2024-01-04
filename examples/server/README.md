# A simple server

This example shows how to create a simple server with Miou. The aim here is to
put tasks in the background and manage incoming connections "at the same time".
So we use an "orphans" value to keep tasks in the background and manage the
acceptance of new connections. You can start the server in this way:
```shell-session
$ dune exec examples/server/main.exe
```


You can launch a client that will connect and disconnect with it:
```shell-session
$ echo "" | netcat localhost 3333
```
