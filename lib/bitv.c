#include <caml/mlvalues.h>

intnat miou_bitv_next_native(value v_buf) {
  intnat i = 0;

#ifdef ARCH_SIXTYFOUR
  uint64_t *d64 = (uint64_t *)Bytes_val(v_buf);

  while (d64[i] == 0xffffffffffffffff)
    i++;
  i *= 2;
#endif

  uint32_t *d32 = (uint32_t *)Bytes_val(v_buf);
  uint16_t *d16 = (uint16_t *)Bytes_val(v_buf);
  uint8_t *d8 = (uint8_t *)Bytes_val(v_buf);

  while (d32[i] == 0xffffffff)
    i++;
  i *= 2;

  while (d16[i] == 0xffff)
    i++;
  i *= 2;

  while (d8[i] == 0xff)
    i++;

  uint8_t v = ~d8[i];

  return (v == 0) ? (i * 8) + 8 : (i * 8) + __builtin_ctz(v);
}

CAMLprim value miou_bitv_next_bytecode(value v_buf) {
  return (Val_long(miou_bitv_next_native(v_buf)));
}

intnat miou_bitv_clz_native(value v_buf) {
  mlsize_t tmp;
  tmp = Bosize_val(v_buf) - 1;
  mlsize_t len = (tmp - Byte(v_buf, tmp));
  if (len == 0)
    return 0;

  uint8_t *d8 = (uint8_t *)Bytes_val(v_buf);
  mlsize_t i = len - 1;

#ifdef ARCH_SIXTYFOUR
  {
    uint64_t *d64 = (uint64_t *)d8;
    mlsize_t j = i / 8;
    while (j > 0 && d64[j] == 0)
      j--;
    i = j * 8 + 7;
    if (i >= len)
      i = len - 1;
  }
#endif

  while (d8[i] == 0 && i > 0)
    i--;

  return (i == 0 && d8[i] == 0) ? 0 : (i * 8) + (32 - __builtin_clz(d8[i]));
}

CAMLprim value miou_bitv_clz_bytecode(value v_buf) {
  return (Val_long(miou_bitv_clz_native(v_buf)));
}
