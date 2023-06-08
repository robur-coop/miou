Unix tests
t01 shows that we actually take the first resolved task and cancel other pending
tasks correctly. It uses our [sleep] syscall instead of [Unix.sleep]
  $ ./t01.exe
