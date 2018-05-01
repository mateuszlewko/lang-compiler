#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gc.h>
#include <assert.h>

// #define malloc(x) GC_MALLOC((x))

typedef unsigned char uchar;
typedef unsigned char byte;

struct thunk {
    void (*fn)();
    uchar *args;
    uchar left_args;
    uchar arity;
    int used_bytes;   
};

/*
val wrappedAdder : int -> int -> int -> int -> int -> int
let wrappedAdder a b c =
    val adder5 : int -> int -> int -> int -> int -> int
    let adder5 a b c d e = a + b + c + d + e
  
    adder5 a b c

let apply4 fn a b c d = fn a b c d
let apply1 
*/

int adder5(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
} 

int lang_entry_value_1adder(int b, int c, int d, int e, byte *data) {
    return adder5(((int*)data)[0], b, c, d, e);
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

void* arr[] = { &pre_adder5, &adder5 };


// int pre_adder5(byte env_args, byte cnt, byte* data,
//                int a, int b, int c, int d, int e)

struct thunk wrapped_adder(int a, int b, int c) {
    struct thunk t;
    t.fn = pre_adder5;
    t.args = malloc(sizeof(int) * 5);
    ((int*)t.args)[0] = a;
    ((int*)t.args)[1] = b;
    ((int*)t.args)[2] = c;
    t.arity = 5;
    t.left_args = 2;
    t.used_bytes = sizeof(int) * 3;

    return t;
}

struct data1 {
    int a, b, c, d, e;
};

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

    // need to apply again
    if ((short)t.left_args < (short)env_args + cnt - 3) {
        byte pass_env_args = t.arity - t.left_args;
        struct thunk (*fn)() = t.fn;

        switch (env_args + cnt - 3) {
            // case 1:
            //     return fn(pass_env_args, 1, t.args, e);
            //     break;
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
        int from_b[] = {0, 4, 8, 12, 16};
        int to_b[] = {0, 4, 8, 12, 16, 20};

        // memcpy to res.args
        struct data1 data = {a, b, c, d, e}; 
        int from = from_b[3 - env_args];
        int b_cnt = to_b[cnt] - from;
        memcpy(t.args + t.used_bytes, ((byte*)&data) + from, 
               b_cnt);
        t.left_args -= env_args + cnt - 3;
        t.used_bytes += b_cnt;
        return t;
    }
}

struct thunk get_adder() {
    struct thunk t;
    t.fn = pre_wrapped_adder;
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
    void (*fn)() = t.fn;

    if (t.left_args == 5) {
        int (*fn2)() = fn;
        return fn2(t.arity - t.left_args, 5, t.args, a, b, c, d, e);
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
            // case 1:
            //     return ((int(*)(byte, byte, byte*, int))(*res.fn))
            //             (env_cnt, 1, res.args, e);
            // case 2:
            //     return ((int(*)(byte, byte, byte*, int, int))(*res.fn)) 
            //             (env_cnt, 2, res.args, d, e);
            // case 3:
            //     return ((int(*)(byte, byte, byte*, int, int, int))(*res.fn))
            //             (env_cnt, 3, res.args, c, d, e);
            // case 4:
            //     return ((int(*)(byte, byte, byte*, int, int, int, int))(*res.fn))
            //             (env_cnt, 4, res.args, b, c, d, e);
            // case 5:
            //     return ((int(*)(byte, byte, byte*, int, int, int, int, int))(*res.fn))
            //             (env_cnt, 5, res.args, a, b, c, d, e);
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
    if (t.left_args > 4) {
        struct thunk res = t;
        res.args = malloc(sizeof(int) * 5);
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
    }
    // apply a b c d e get int
    // if too many args 
    //     put extra args on stack
    //     call t.fn with required args and extra args count
    //     get last thunk
    //     call thunk with only env (data / args) and return
    // return -1;
}

int applyI (struct thunk t /* fn : int -> int */, int e) {
   // if (th.left_args == 1) {
    int (*fn)(byte, byte, byte*, int) = t.fn;
    return fn(t.arity - t.left_args, 1, t.args, e);
    // } 
}

int gg = 123;

void perf_test() {
    int x = rand() % 3 + 4;

    for (int i = 0; i < 1; i++) {
        struct thunk t = get_adder();
        gg ^= applyIIIII(t, i * x, i + x + 5, i * i + x, i, i + x);
        // struct thunk res1 = applyIIII(t, i * x, i + x + 5, i * i + x, i);
        // gg ^= applyI(res1, i + x);
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
    test_new();
    test_new2();

    // perf_test();
}