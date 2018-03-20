#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gc.h>

#define malloc(x) GC_MALLOC((x))


/*
let complexAdder add x y =
    x + y + add x y

let getAdder x = 
    let adder x y z = 
        let innerAdder x y z w q = x + y + z + w + q
        in innerAdder x y z 
    in adder x 
*/

// int call_fn(int fn(), int cnt) {
//     return cnt == 3 ? fn(1, 2, 30) : fn(15, 120);
// }

// int add3_slim(int x, int y, int z) {
//     return x + y + z;
// }

// int add2_slim(int x, int y) {
//     return x + y;
// }

// int innerAdder(int x, int y, int z, int w, int q) {
//     return x + y + z + w + q;
// }

// struct thunk adder(int x, int y, int z) {
//     struct thunk th;
    
//     th.fn = &innerAdder;
//     th.left_args = 2;
//     th.arity = 5;
//     th.args = malloc(sizeof(int) * 3);
//     th.args[0] = x;
//     th.args[4] = y;
//     th.args[8] = z;

//     return th;
// }

// // struct thunk* pre_adder(char* data) {
// //     int* args = (int*)data;
// //     return adder(args[0], args[1], args[2]);
// // }

// struct thunk getAdder(int x) {
//     struct thunk th;
    
//     th.fn = &adder;
//     th.left_args = 2;
//     th.arity = 3;
//     th.args = malloc(sizeof(int));
//     th.args[0] = x;

//     return th;
// }

// // struct thunk apply(struct thunk th, )

// struct thunk single(int x, int y, int z, int w, int q) {
//     struct thunk th = getAdder(x);

//     int data[] = {y, z, w, q};
//     int cnt = 0;

//     START:

//     switch (th.left_args) {
//         case 1:
//         break;
//         case 2:
//             switch (th.arity) {
//                 case 2: return th.fn(y, z, w);
//                 break;
//                 case 3: 
//                     th = th.fn(((int*)th.args)[0], data[cnt], data[cnt + 1]);
//                 break;
//                 case 4: return th.fn(((int*)th.args)[0], ((int*)th.args)[1], y);
//                 break;
//             }
//         break;
//         case 3:
//             // switch (th.arity - th.left_args) {
//             //     case 0:
//             //         return th.fn(y, z, w);
//             //     break;
//             //     case 1:
//             //         return th.fn(((int*)th.args)[0], y, z);
//             //     break;
//             //     case 2:
//             //         return th.fn(((int*)th.args)[0], ((int*)th.args)[1], y);
//             //     break;
//             // }
//         break;
//         case 4: 
//         break;
//     }
// }

// int gg = 123;

// void test() {
//     int x = rand() % 10 + 4;

//     for (int i = 0; i < 100000000; i++) {
//         struct thunk th = single(i * x, i + x + 5, i * i - x, 2, 3 * x);
//         gg ^= ((int*)th.args)[0];
//         // gg ^= ((int*)th.args)[2];
//     }

//     printf("%d\n", gg);
// }

// struct three {
//     long a;
//     int b;
//     long c;
// };

// volatile struct three no_ret(long a, int b, long c) {
//     struct three t = {a, b, c};
//     return t;
// }

// volatile struct three fake_ret() {
    
// }


typedef unsigned char uchar;
typedef unsigned char byte;

struct thunk {
    void (**fn)();
    uchar *args;
    uchar left_args;
    uchar arity;
    int used_bytes;   
};

volatile int adder5(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
}

int pre_adder5_1(int e, byte *data) {
    return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], 
                  ((int*)data)[3], e);
}

int pre_adder5_2(int d, int e, byte *data) {
    return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], d, e);
}

/// 5_3 ... 5_4

void (*adder5_ptrs[])() = {(void(*))adder5, 
                           (void(*))pre_adder5_2, 
                           (void(*))pre_adder5_1};

inline struct thunk wrapped_adder(int a, int b, int c) {
    struct thunk t;
    t.fn = &(adder5_ptrs[2]);
    t.args = malloc(sizeof(int) * 5);
    ((int*)t.args)[0] = a;
    ((int*)t.args)[1] = b;
    ((int*)t.args)[2] = c;
    t.arity = 5;
    t.left_args = 2;
    t.used_bytes = sizeof(int) * 3;

    return t;
}

inline struct thunk wrapped_adder_1(int c, byte *data) {
    return wrapped_adder(((int*)data)[0], ((int*)data)[1], c);
}

inline struct thunk wrapped_adder_2(int b, int c, byte *data) {
    return wrapped_adder(((int*)data)[0], b, c);
}

void (*wrapped_adder_ptrs[])() = {(void(*))wrapped_adder, 
                                  (void(*))wrapped_adder_2, 
                                  (void(*))wrapped_adder_1};

inline struct thunk get_adder() {
    struct thunk t;
    t.fn = &(wrapped_adder_ptrs[0]);
    t.args = malloc(sizeof(int) * 3);
    // ((int*)t.args)[0] = a;
    // ((int*)t.args)[1] = b;
    // ((int*)t.args)[2] = c;
    t.arity = 3;
    t.left_args = 3;
    t.used_bytes = 0;

    return t;
}

int applyIIIII(struct thunk t /* int -> int -> int -> int -> int -> int */, 
                    int a, int b, int c, int d, int e) /* -> int */ {
    // apply a b c d e get int
    // if too many args 
    //     put extra args on stack
    //     call t.fn with required args and extra args count
    //     get last thunk
    //     call thunk with only env (data / args) and return
    return 0;
}

int applyIIII(struct thunk t /* int -> int -> int -> int */, 
                    int c, int d, int e) /* -> int */ {
    // apply c d e get int
    return 0;
}

struct thunk applyII(struct thunk t /* int -> int -> int -> int -> int */, 
                    int a, int b) /* -> int -> int -> int -> int */ {
    // apply a b get thunk
    return t;
}

int applyIII(struct thunk t /* int -> int -> int -> int */, 
                    int c, int d, int e) /* -> int */ {
    // apply c d e get int
    return 0;
}

int applyI (struct thunk th /* fn : int -> int */, int c) {
   // if (th.left_args == 1) {
        void (*fn)() = *th.fn;
        int (*fn2)() = fn;
        return fn2(c, th.args);
    // } 
}



int gg = 123;

inline int test1(int a, int b, int c) {
    struct thunk t = applyII(a, b);
    return applyI(t, c);
    // printf("%d\n", res);
}

void perf_test() {
    int x = rand() % 10 + 4;

    for (int i = 0; i < 100000000; i++) {
        gg ^= test1(i * x, i + x + 5, i * i - x);
        // gg ^= ((int*)th.args)[0];
        // gg ^= ((int*)th.args)[2];
    }

    printf("%d\n", gg);
}



int main() {
    // test1();
    perf_test();

    // int res = call_fn(add3_slim, 3);
    // printf("%d\n", res);

    // res = call_fn(add2_slim, 2);
    // printf("%d\n", res);
    // int x;
    // scanf("%d", &x);

    // test();

    // void (*fn)() = no_ret;


    // long a; int b; long c;
    // scanf("%lld %d %lld", &a, &b, &c);
    
    // if (a % 1000 == 0) {
    //     struct three (*fn2)() = fn;
    //     struct three t = fn2(a, b, c);
    //     printf("%lld %d %lld\n", t.a, t.b, t.c);
    // }
    
    // no_ret(a, b, c);
    // long t = fn(a, b, c);


    // struct thunk th = single(i * x, i + x + 5, i * i - x, 2, 3 * x);
    // printf("left_args: %d, arity: %d\n x0: %d, x1: %d, x2: %d\n", 
    //        th.left_args, th.arity, ((int*)th.args)[0], ((int*)th.args)[1], 
    //                                ((int*)th.args)[2]); 
}