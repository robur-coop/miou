Unix tests
t01 shows that we actually take the first resolved task and cancel other pending
tasks correctly. It uses our [sleep] syscall instead of [Unix.sleep]
  $ ./t01.exe
t02 shows that if we cancel a task, we must care about its children and find a
mechanism to properly await or cancel these children
  $ ./t02.exe
  Fatal error: exception Miou.Still_has_children
  [2]
It shows the cancellation mechanism between domains and blocking operations
  $ ./t04.exe
  $ ./t05.exe
  Fatal error: exception Miou.Still_has_children
  [2]
