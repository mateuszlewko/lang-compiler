#include <stdio.h>

int fun(int a)
{
    return a * 2;
}

int test(int (*fun_ptr)(int)) {
    // printf("%d %d", a, b);
    return (*fun_ptr)(10);
}

int main()
{
   printf("%d", test(&fun));
    return 0;
}