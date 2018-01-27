#include <stdlib.h>
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

extern int ll_get_ith_elem_of_int_array(int* arr, int i) {
  return *(arr + i);
}

extern void ll_set_ith_elem_of_int_array(int* arr, int i, int val) {
  *(arr + i) = val;
}

extern int* ll_new_int_array(int size) {
  return (int *)malloc(sizeof(int) * size);
}

extern void ll_print_bool(bool x) {
  printf(x ? "true" : "false");
}