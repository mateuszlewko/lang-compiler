#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gc.h>

#define malloc(x) GC_MALLOC((x))

typedef unsigned char uchar;

struct thunk {
    void (**fn)();
    uchar *args;
    uchar left_args;
    uchar arity;
    int used_bytes;   
};

void (**fn_ptrs)() = {call_fn, add3_slim};

/*
let complexAdder add x y =
    x + y + add x y

let getAdder x = 
    let adder x y z = 
        let innerAdder x y z w q = x + y + z + w + q
        in innerAdder x y z 
    in adder x 
*/

int call_fn(int fn(), int cnt) {
    return cnt == 3 ? fn(1, 2, 30) : fn(15, 120);
}

int add3_slim(int x, int y, int z) {
    return x + y + z;
}

int add2_slim(int x, int y) {
    return x + y;
}

int innerAdder(int x, int y, int z, int w, int q) {
    return x + y + z + w + q;
}

struct thunk adder(int x, int y, int z) {
    struct thunk th;
    
    th.fn = &innerAdder;
    th.left_args = 2;
    th.arity = 5;
    th.args = malloc(sizeof(int) * 3);
    th.args[0] = x;
    th.args[4] = y;
    th.args[8] = z;

    return th;
}

// struct thunk* pre_adder(char* data) {
//     int* args = (int*)data;
//     return adder(args[0], args[1], args[2]);
// }

struct thunk getAdder(int x) {
    struct thunk th;
    
    th.fn = &adder;
    th.left_args = 2;
    th.arity = 3;
    th.args = malloc(sizeof(int));
    th.args[0] = x;

    return th;
}

// struct thunk apply(struct thunk th, )

struct thunk single(int x, int y, int z, int w, int q) {
    struct thunk th = getAdder(x);

    int data[] = {y, z, w, q};
    int cnt = 0;

    START:

    switch (th.left_args) {
        case 1:
        break;
        case 2:
            switch (th.arity) {
                case 2: return th.fn(y, z, w);
                break;
                case 3: 
                    th = th.fn(((int*)th.args)[0], data[cnt], data[cnt + 1]);
                break;
                case 4: return th.fn(((int*)th.args)[0], ((int*)th.args)[1], y);
                break;
            }
        break;
        case 3:
            // switch (th.arity - th.left_args) {
            //     case 0:
            //         return th.fn(y, z, w);
            //     break;
            //     case 1:
            //         return th.fn(((int*)th.args)[0], y, z);
            //     break;
            //     case 2:
            //         return th.fn(((int*)th.args)[0], ((int*)th.args)[1], y);
            //     break;
            // }
        break;
        case 4: 
        break;
    }
}

int gg = 123;

void test() {
    int x = rand() % 10 + 4;

    for (int i = 0; i < 100000000; i++) {
        struct thunk th = single(i * x, i + x + 5, i * i - x, 2, 3 * x);
        gg ^= ((int*)th.args)[0];
        // gg ^= ((int*)th.args)[2];
    }

    printf("%d\n", gg);
}

struct three {
    long a;
    int b;
    long c;
};

volatile struct three no_ret(long a, int b, long c) {
    struct three t = {a, b, c};
    return t;
}

volatile struct three fake_ret() {
    
}

int main() {
    // int res = call_fn(add3_slim, 3);
    // printf("%d\n", res);

    // res = call_fn(add2_slim, 2);
    // printf("%d\n", res);
    // int x;
    // scanf("%d", &x);

    // test();

    void (*fn)() = no_ret;


    long a; int b; long c;
    scanf("%lld %d %lld", &a, &b, &c);
    
    if (a % 1000 == 0) {
        struct three (*fn2)() = fn;
        struct three t = fn2(a, b, c);
        printf("%lld %d %lld\n", t.a, t.b, t.c);
    }
    
    // no_ret(a, b, c);
    // long t = fn(a, b, c);


    // struct thunk th = single(i * x, i + x + 5, i * i - x, 2, 3 * x);
    // printf("left_args: %d, arity: %d\n x0: %d, x1: %d, x2: %d\n", 
    //        th.left_args, th.arity, ((int*)th.args)[0], ((int*)th.args)[1], 
    //                                ((int*)th.args)[2]); 
}