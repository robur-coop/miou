Simple tests
t01 is a basic test where we launch 2 continuations and add result of them
  $ ./t01.exe
  3
t02 shows that 2 continuations are executed according to a **random** order
  $ ./t02.exe > out.txt
  $ test "$(cat out.txt)" = "1" -o "$(cat out.txt)" = "2"
t03 shows that we **must** await all children of a continuation
  $ ./t03.exe
  Fatal error: exception Miou.Still_has_children
  [2]
t04 shows that if a task fails, all children are cancelled and the exception
is returned. The test randomly fails.
  $ ./t04.exe > out.txt
  $ test -z "$(cat out.txt)" -o "$(cat out.txt)" = "Failed!"
t05 shows that we can cancel a task (in parallel or concurrently). It shows that
the task was launched but ensure that we did not spend 10s - because we
cancelled the [Unix.sleep]
  $ ./t05.exe
  Launched!
t06 shows that we actually take the first resolved task and cancel other pending
tasks correctly.
  $ ./t06.exe
t07 shows that it is not possible to await a task that is not our children
  $ ./t07.exe
  Fatal error: exception Miou.Not_a_child
  [2]
t08 shows that we execute 2 [Unix.sleep] in parallel and we should spend only
1 second for both of them
  $ ./t08.exe
t09 is like t03 but for domains, we must wait all children
  $ ./t09.exe
  Launched!
  Fatal error: exception Miou.Still_has_children
  [2]
t10 is a bit more complex where 2 domains want to share the same task. Only one
can [await] the shared task and we check that we actually return an error here
  $ ./t10.exe
  Fatal error: exception Miou.Not_a_child
  [2]
t11 is a special case where we give an opportunity for tasks to be resolved.
However, we does not consume them so we still have the [Still_hash_children]
issue
  $ ./t11.exe
  Fatal error: exception Miou.Still_has_children
  [2]
t12 shows that [cancel] does not make a transition state when a promise is
already resolved
  $ ./t12.exe
