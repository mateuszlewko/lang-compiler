#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gc.h>

typedef unsigned char uchar;

struct thunk {
    void* (*fn)(char*);
    char* args;
    uchar left_arity;
    size_t curr_bytes;
    size_t needed_bytes;
};

#define malloc(x) GC_MALLOC((x))

/*
let getAdder x = 
    let adder x y z = 
        let innerAdder x y z w q = x + y + z + w + q
        in innerAdder x y z 
    in adder x 
*/

int innerAdder(int x, int y, int z, int w, int q) {
    return x + y + z + w + q;
}

int pre_innerAdder(char* data) {
    int* args = (int*)data;
    return innerAdder(args[0], args[1], args[2], args[3], args[4]);
}

struct thunk* adder(int x, int y, int z) {
    struct thunk* th = malloc(sizeof(struct thunk));
    
    th->fn = &pre_innerAdder;
    th->left_arity = 2;
    th->args = malloc(sizeof(int) * 3);
    th->curr_bytes = sizeof(int) * 3;
    th->needed_bytes = sizeof(int) * 5;

    int data[] = {x, y, z};

    memcpy(th->args, (void*)data, sizeof(int) * 3);
    return th;
}

struct thunk* pre_adder(char* data) {
    int* args = (int*)data;
    return adder(args[0], args[1], args[2]);
}

struct thunk* getAdder(int x) {
    struct thunk* th = malloc(sizeof(struct thunk));
    
    th->fn = &pre_adder;
    th->left_arity = 2;
    th->args = malloc(sizeof(int));
    th->curr_bytes = sizeof(int);
    th->needed_bytes = sizeof(int) * 3;

    memcpy(th->args, &x, sizeof(int));
    return th;
}

struct thunk* pre_getAdder(char* data) {
    return getAdder(((int*)data)[0]);
}

char* generic_apply(struct thunk* th, uchar cnt, char* args, size_t args_size) {

    // copy all required bytes to one array
    size_t diff_bytes = th->needed_bytes - th->curr_bytes;
    size_t used_bytes = args_size < diff_bytes ? args_size : diff_bytes;

    char* final_args = malloc(th->curr_bytes + used_bytes);
    memcpy(final_args, th->args, th->curr_bytes);
    memcpy(final_args + th->curr_bytes, args, used_bytes);

    if (th->left_arity <= cnt) {
        // pass thunk with all args (as byte array) to th->fn
        if (used_bytes == args_size) 
            return (char*)(th->fn(final_args));
        else {
            struct thunk* mid_th = (struct thunk*)th->fn(final_args);
            return generic_apply(mid_th, 
                                 cnt - th->left_arity,
                                 args + used_bytes, args_size - used_bytes);
        }
    }
    else {
        struct thunk* res = malloc(sizeof(struct thunk));

        res->left_arity = th->left_arity - cnt;
        res->args = final_args;
        res->fn = th->fn;
        res->curr_bytes = th->curr_bytes + args_size;
        res->needed_bytes = th->needed_bytes;

        return (char*)res;
    }
}

struct thunk g;

int x = 2;

void __attribute__((always_inline)) test(int i) {
    struct thunk* th = getAdder(1<<20);
    int next_args[] = {2, 15, i};
    struct thunk* res1 = (struct thunk*)generic_apply(th, 3, (char*)&next_args, sizeof(int) * 3);

    x ^= ((int*)res1->args)[3];

    // for (int i = 0; i < 4; i++) {
    //     printf("%d\n", ((int*)res1->args)[i]);
    // }
}

int fast_add2(int a, int b, int c, int d) {
    x ^= b;
    return a + b + c + 3 * d;
}
int fast_add1(int a, int b, int c, int d) {
    x ^= fast_add2(a, b, c, d);
    return a * b + c - d + fast_add2(a, b, c, d);
}
int fast_add(int a, int b, int c, int d) {
    x ^= fast_add1(a, b, c, d);
    return a + b + c - d + fast_add1(a, b, c, d);
}

int main() {
    for (int i = 0; i < 100000000; i++)
        test(i);

    printf("%d\n", x);
}