# DNS resolution

This example shows the implementation of a "pool" of connections. In other
words, we try several connections and choose the one that finishes its TCP/IP
handshake as quickly as possible. A DNS resolution is performed. The idea is to
allocate a "daemon" to manage all the TCP/IP connections (which is the role of
happy-eyeballs). As soon as it is switched off, it makes sure that all the
resources have been released.

You can test the code with it:
```shell-session
$ dune exec examples/happy/main.exe --
Connected to google.com via *:443
...
```
