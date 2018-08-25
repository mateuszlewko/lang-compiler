#include <stdio.h>
#include <string.h>
#include <stdlib.h> 

int* get_arr() {
  int arr[] = {1, 4, 19};
  int* res = (int*)malloc(4 * 3);
  memcpy(res, arr, 4 * 3);
  return res;
}

int main() {
  int* a = get_arr();
  printf("%d\n", *(a + 1));
}
