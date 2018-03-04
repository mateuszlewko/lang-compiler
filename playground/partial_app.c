#include <stdio.h>
#include <stdlib.h>

typedef unsigned char uchar;

struct thunk {
    uchar left_arity;
    char** args;
    void* fn;
};

struct arg2 {
    char x1;
    float x2;
};

struct add_args {
    int x1;
    int* x2;
};

int pre_add(char** args) {
    struct add_args proper_args;
    memcpy(&proper_args, args, sizeof(struct add_args));
    add(proper_args.x1, proper_args.x2);
}

int add(int a, int* b) {
    printf("%d\n", a + *b);
    *b = 106;
    return a + *b;
}

struct thunk* generic_apply(struct thunk* th, uchar cnt, char** args) {
    if (th->left_arity <= cnt) {
        char* final_args = malloc(sizeof(th->args));
        // copy all required bytes to one array
        // pass thunk with all args (as byte array) to th->fn
    }
}

void* apply2 (void* (*fn)(void*, void*), void** args) {
    return fn(args[0], args[1]);
}

void* some_apply(void* some, void** args) {
    void* (*fn)(void*, void*) = some;
    apply2(fn, args);
}

int main() {
    void** a = malloc(2 * sizeof(void*));

    a[0] = 3;
    // a[1] = malloc(sizeof(int));
    int* x = malloc(sizeof(int));
    *x = 64;

    *(a + 1) = x;

    int* xc = *(a + 1);

    int res = some_apply(add, a);

    printf("after: %d", *((int*)*(a + 1)));
}