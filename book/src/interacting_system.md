# Interacting with the system

In the implementation of our `echo` server, beyond the question of
synchronicity, there were also considerations about interactions with the system
and the resources it provides, such as sockets.

We also observed that to manipulate these resources, we had functions that we
described as "blocking" meaning they waited for an event to occur (such as the
connection of a new client) before unblocking. In addition to wanting to put
tasks in the background, we also wanted to take the opportunity to do something
else if we encountered such a function.

We have this opportunity with `Await`, which observes the state of our promise
and then decides to continue if it contains the result of our task or to "yield"
(meaning, execute other tasks) if the associated task is not yet complete.

We could reproduce the same approach for these blocking functions: continue if
they have an event to notify us about, or "yield" if we know they will block.
The crucial question then is to know in advance if they will block before
actually blocking. The system can provide us with such information.

## File-descriptors


