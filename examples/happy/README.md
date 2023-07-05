# A DNS resolver with our scheduler

DNS resolution can be a tricky problem, depending on what you want to offer the
user. Using `gethostbyname` can involve several processes:
- a pool of available connections
- a process that sends and receives DNS requests

These processes are "in the background". In other words, the user will interact
with these processes, but they will have already been launched. This raises a
number of questions:
- how to clean up these processes (as well as open connections)
- how to react to abnormal cases?
- can these processes run in parallel?

This little program shows an example of how to make such a program. It has the
advantage of launching our process, which will connect to the servers, in the
background, as well as launching a task (if a connection is available) to
communicate with one of the servers.

This explicitly requires us to release the resources at the end of our program.
We must "kill" our daemons.
