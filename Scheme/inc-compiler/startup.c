#include <stdio.h>

#define bool_f      0x2F
#define bool_t      0x6F
#define nil_list    0x3F
#define fx_mask     0x03
#define fx_tag      0x00
#define fx_shift       2
#define ch_mask     0x0F
#define ch_tag      0x0F
#define ch_shift       8

typedef unsigned int ptr;

static void print_ptr(ptr x) {
  if ((x & fx_mask) == fx_tag) {
    printf("%d", ((int) x) >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == nil_list) {
    printf("()");
  } else if ((x & ch_mask) == ch_tag) {
    char c = (char) (x >> ch_shift);
    if (c == '\n')      printf("#\\newline");
    else if (c == '\t') printf("#\\tab");
    else if (c == '\r') printf("#\\return");
    else if (c == ' ')  printf("#\\space");
    else                printf("#\\%c", c);
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}

ptr scheme_entry();

int main(int argc, char** argv) {
  print_ptr(scheme_entry());
  return 0;
}
