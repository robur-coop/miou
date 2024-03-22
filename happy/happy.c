#include <caml/memory.h>
#include <caml/fail.h>

extern int caml_unix_socket_type_table[];

CAMLprim value
happy_translate_so_type(value so_type) {
  int n = Int_val (so_type);
  int r = -1;

  for (int i = 0; i < 4; i++)
    if (caml_unix_socket_type_table[i] == n)
      r = i;

  if (r == -1)
    caml_invalid_argument("invalid type of socket");

  return Val_int (r);
}
