#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gc.h>
#include <assert.h>

#define malloc(x) GC_MALLOC((x))


/*
let complexAdder add x y =
    x + y + add x y

let rr = ref 0

let getAdder2 x = 
    rr := !rr + x;
    let adder x y z = 
        rr := !rr + x + z;
        let innerAdder x y z w q = 
            rr := !rr + x + y + z + q;
            x + y + z + w + q
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

// #define STACK_SIZE 100000
// byte stack[STACK_SIZE];
// size_t sp = 0;

struct thunk {
    void (*fn)();
    uchar *args;
    uchar left_args;
    uchar arity;
    int used_bytes;   
};

int adder5(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
}

int pre_adder5(byte env_args, byte cnt, byte* data,
                      int a, int b, int c, int d, int e) {
    switch (env_args) {
        case 0:
            return adder5(a, b, c, d, e);
            break;
        case 1:
            return adder5(((int*)data)[0], a, b, c, d);
            break;
        case 2:
            return adder5(((int*)data)[0], ((int*)data)[1], a, b, c);
            break;
        case 3:
            return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], 
                          a, b);
            break;
        case 4:
            return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], 
                          ((int*)data)[3], a);
        case 5:
            return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], 
                          ((int*)data)[3], ((int*)data)[4]);
            break;
    }
}

// int pre_adder5_1(int e, byte *data) {
//     return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], 
//                   ((int*)data)[3], e);
// }

// int pre_adder5_2(int d, int e, byte *data) {
//     return adder5(((int*)data)[0], ((int*)data)[1], ((int*)data)[2], d, e);
// }

/// 5_3 ... 5_4

// void (*adder5_ptrs[])() = {(void(*))adder5, 
//                            (void(*))pre_adder5_2, 
//                            (void(*))pre_adder5_1};

struct thunk wrapped_adder(int a, int b, int c) {
    struct thunk t;
    t.fn = pre_adder5;
    t.args = malloc(sizeof(int) * 3);
    ((int*)t.args)[0] = a;
    ((int*)t.args)[1] = b;
    ((int*)t.args)[2] = c;
    t.arity = 5;
    t.left_args = 2;
    t.used_bytes = sizeof(int) * 3;

    return t;
}

struct thunk pre_wrapped_adder(byte env_args, byte cnt, byte* data,
                               int a, int b, int c, int d, int e) {
    struct thunk t;
    switch (env_args) {
        case 0:
            t = wrapped_adder(a, b, c);
            break;
        case 1:
            t = wrapped_adder(((int*)data)[0], a, b);
            break;
        case 2:
            t = wrapped_adder(((int*)data)[0], ((int*)data)[1], a);
            break;
    }

    if ((short)t.left_args < (short)env_args + cnt - 3) {
        byte pass_env_args = t.arity - t.left_args;
        struct thunk (*fn)() = t.fn;

        switch (env_args + cnt - 3) {
            case 1:
                return fn(pass_env_args, 1, t.args, e);
                break;
            case 2:
                return fn(pass_env_args, 2, t.args, d, e);
                break;
            case 3:
                return fn(pass_env_args, 3, t.args, c, d, e);
                break;
            case 4:
                return fn(pass_env_args, 4, t.args, b, c, d, e);
                break;
            case 5:
                return fn(pass_env_args, 5, t.args, a, b, c, d, e);
                break;
        }
    }
    else {
        // memcpy to res.args
        int data[] = {a, b, c, d, e}; 
        memcpy(t.args + t.used_bytes, &data[3 - env_args], 
               sizeof(int) * (env_args + cnt - 3));
        t.left_args -= env_args + cnt - 3;
        t.used_bytes += sizeof(int) * (env_args + cnt - 3);
        return t;
    }
}

// inline struct thunk wrapped_adder_1(int c, byte *data) {
//     return wrapped_adder(((int*)data)[0], ((int*)data)[1], c);
// }

// inline struct thunk wrapped_adder_2(int b, int c, byte *data) {
//     return wrapped_adder(((int*)data)[0], b, c);
// }

// void (*wrapped_adder_ptrs[])() = {(void(*))wrapped_adder, 
//                                   (void(*))wrapped_adder_2, 
//                                   (void(*))wrapped_adder_1};

struct thunk get_adder() {
    struct thunk t;
    t.fn = pre_wrapped_adder;
    // t.args = malloc(sizeof(int) * 3);
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
    void (*fn)() = t.fn;

    if (t.left_args == 5) {
        int (*fn2)() = fn;
        fn2(t.arity - t.left_args, 5, t.args, a, b, c, d, e);
    }
    else {
        struct thunk (*fn2)(byte, byte, byte*, int, int, int, int, int) = fn;
        struct thunk res = fn2(t.arity - t.left_args, (uchar)5, t.args, a, b, c, 
                               d, e);
                               
        byte env_cnt = res.arity - res.left_args;

        switch (res.left_args) {
            case 0:
                return ((int(*)(byte, byte, byte*))(*res.fn))
                        (env_cnt, 0, res.args);
            case 1:
                return ((int(*)(byte, byte, byte*, int))(*res.fn))
                        (env_cnt, 1, res.args, e);
            case 2:
                return ((int(*)(byte, byte, byte*, int, int))(*res.fn)) 
                        (env_cnt, 2, res.args, d, e);
            case 3:
                return ((int(*)(byte, byte, byte*, int, int, int))(*res.fn))
                        (env_cnt, 3, res.args, c, d, e);
            case 4:
                return ((int(*)(byte, byte, byte*, int, int, int, int))(*res.fn))
                        (env_cnt, 4, res.args, b, c, d, e);
            case 5:
                return ((int(*)(byte, byte, byte*, int, int, int, int, int))(*res.fn))
                        (env_cnt, 5, res.args, a, b, c, d, e);
        }
    }
    // apply a b c d e get int
    // if too many args 
    //     put extra args on stack
    //     call t.fn with required args and extra args count
    //     get last thunk
    //     call thunk with only env (data / args) and return
    return -1;
}

struct thunk applyIIII(struct thunk t /* int -> int -> int -> int -> int -> int */, 
                       int a, int b, int c, int d) /* -> int -> int */ {
    // void (*fn)() = t.fn;

    if (t.left_args > 4) {
        struct thunk res = t;
        res.args = malloc(sizeof(int) * 4);
        memcpy(res.args, t.args, t.used_bytes);
        ((int*)(res.args + t.used_bytes))[0] = a;
        ((int*)(res.args + t.used_bytes))[1] = b;
        ((int*)(res.args + t.used_bytes))[2] = c;
        ((int*)(res.args + t.used_bytes))[3] = d;
        res.left_args -= 4;
        res.used_bytes += sizeof(int) * 4;

        return res;
    }
    else {
        struct thunk (*fn2)(byte, byte, byte*, int, int, int, int) = t.fn;
        struct thunk res = fn2(t.arity - t.left_args, (uchar)4, t.args, a, b, c, 
                               d);
        return res;
        // byte env_cnt = res.arity - res.left_args;

        // switch (res.left_args) {
        //     case 0:
        //         return res;
        //     case 1:
        //         ((int*)(res.args + res.used_bytes))[0] = d;
        //         res.used_bytes += sizeof(int);
        //         res.left_args -= 1;
        //         return res;
        //     case 2:
        //         ((int*)(res.args + res.used_bytes))[0] = c;
        //         ((int*)(res.args + res.used_bytes))[1] = d;
        //         res.used_bytes += sizeof(int) * 2;
        //         res.left_args -= 2;
        //         return res;
        //     case 3:
        //         ((int*)(res.args + res.used_bytes))[0] = b;
        //         ((int*)(res.args + res.used_bytes))[1] = c;
        //         ((int*)(res.args + res.used_bytes))[2] = d;
        //         res.used_bytes += sizeof(int) * 3;
        //         res.left_args -= 3;
        //         return res;
        //     case 4:
        //         assert(0);
        // }
    }
    // apply a b c d e get int
    // if too many args 
    //     put extra args on stack
    //     call t.fn with required args and extra args count
    //     get last thunk
    //     call thunk with only env (data / args) and return
    // return -1;
}

// int applyIIII(struct thunk t /* int -> int -> int -> int */, 
//                     int c, int d, int e) /* -> int */ {
//     // apply c d e get int
//     return 0;
// }

// struct thunk applyII(struct thunk t /* int -> int -> int -> int -> int */, 
//                     int a, int b) /* -> int -> int -> int -> int */ {
//     // apply a b get thunk
//     return t;
// }

// int applyIII(struct thunk t /* int -> int -> int -> int */, 
//                     int c, int d, int e) /* -> int */ {
//     // apply c d e get int
//     return 0;
// }

int applyI (struct thunk t /* fn : int -> int */, int e) {
   // if (th.left_args == 1) {
    int (*fn)(byte, byte, byte*, int) = t.fn;
    return fn(t.arity - t.left_args, 1, t.args, e);
    // } 
}

int gg = 123;

// inline int test1(int a, int b, int c) {
//     // struct thunk t = applyII(a, b);
//     // return applyI(t, c);
//     // printf("%d\n", res);
// }

void perf_test() {
    int x = rand() % 3 + 4;

    for (int i = 0; i < 100000000; i++) {
        struct thunk t = get_adder();
        struct thunk res1 = applyIIII(t, i * x, i + x + 5, i * i + x, i);
        gg ^= applyI(res1, i + x);

        // struct thunk t = get_adder();
        // gg ^= applyIIIII(t, i * x, i + x + 5, i * i + x, i, i + x);
        // gg ^= test1(i * x, i + x + 5, i * i - x);
        // gg ^= ((int*)th.args)[0];
        // gg ^= ((int*)th.args)[2];
    }

    printf("%d\n", gg);
}

// int test_pass(int cnt, int a, int b, int c, int d) {
//     switch (cnt) {
//         case 1:
//             return a;
//         case 2:
//             return a + b;
//         case 3:
//             return a + b + c;
//         case 4:
//             return a + b + c + d;
//     }
// }

void test_new() {
    struct thunk t = get_adder();
    int res = applyIIIII(t, 1, 3, 5, 7, 9);

    printf("%d\n", res);
}

void test_new2() {
    struct thunk t = get_adder();
    struct thunk res1 = applyIIII(t, 1, 3, 5, 7);
    int res = applyI(res1, 9);
    printf("%d\n", res);
}

int main() {

    perf_test();
    // test_new2();

    // test1();
    // perf_test();
    // int (*fn1)(int, int) = &test_pass;
    // int res1 = fn1(1, 3); 
    // printf("%d\n", res1);

    // int (*fn3)(int, int, int, int) = &test_pass;
    // int res3 = fn3(3, 3, 5, 34000000); 
    // printf("%d\n", res3);



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