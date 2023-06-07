Simple test
  $ ./t01.exe
  3
  $ ./t02.exe > out.txt
  $ test "$(cat out.txt)" = "1" -o "$(cat out.txt)" = "2"
  $ ./t03.exe
  Fatal error: exception Miou.Still_has_children
  [2]
  $ ./t04.exe > out.txt
  $ test -z "$(cat out.txt)" -o "$(cat out.txt)" = "Failed!"
  $ ./t05.exe
  Launched!
  $ ./t07.exe
  Fatal error: exception Miou.Not_a_child
  [2]
