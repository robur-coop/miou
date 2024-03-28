# Synchronous OCaml

In this chapter, we'll explain what synchronous programming is, why we need it,
and briefly discuss some of the ways asynchronous functions have historically
been implemented in Miou.

Asynchronous programming is a technique that enables your program to start a
potentially long-running task and still be able to be responsive to other events
while that task runs, rather than having to wait until that task has finished.
Once that task has finished, your program is presented with the result.

Some functions provided by Miou, can potentially take a long time, and
therefore, are asynchronous. For example:
- `Miou_unix.read` which attemps to read some bytes from a socket
- `Miou_unix.connect` which connects a socket to an address

So even though you may not have to implement your own asynchronous functions
very often, you are very likely to need to use them correctly.

In this chapter, we'll start by looking at the problem with long-running
synchronous functions, which make asynchronous programming a necessity.


