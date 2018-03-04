#include <stdio.h>

struct S {
    const long a1;
    long a2;
    long a3;
    long a4;
    long a5;
    long a6;
    long a7;
    long a8;
};

struct S foo() {
    struct S s = {3, 5};
    s.a2 = 2;

    s.a2 = 9;
    return s;
}

int side_eff(int x) {
    printf("side effect\n");
    return x;
}

int main() {

    struct S acopy = foo();
    long aa = acopy.a1;

    printf("%lld\n", aa);
}