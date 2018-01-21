#include <stdio.h>

struct S {
    int a;
    int b;
    int c;
    int g[10];
};

struct S a = {1, 2, 3, 0};
int b;

int main()
{
    a.a = 5;
    b = 11;
//    printf("%d", test(&fun));
    return 0;
}