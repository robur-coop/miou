# The dining philosophers problem

This is a basic example of how to launch several domains with miou. So, this
program waits for an argument, the time the user wants to spend watching
philosophers eat and think. You can run it like this (if you want to wait 30
seconds):
```shell-session
$ dune exec test/philosophers/main.exe -- 30
```

The program mainly shows the timeout design: launch an operation and `cancel` it
if you've waited too long. `miou` cancels all tasks as soon as one has finished.
