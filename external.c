#include <stdio.h>
#include <stdbool.h>

extern void ll_putint(int x) {
  printf("%d", x);
}

extern void ll_putchar(char x) {
  printf("%c", x);
}

extern void ll_print_line() {
  printf("\n");
}

extern void ll_print_bool(bool x) {
  printf(x ? "true" : "false");
}

// int main () {}