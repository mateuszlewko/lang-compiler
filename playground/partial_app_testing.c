#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

typedef unsigned char uchar;

struct thunk {
    void* (*fn)(char**);
    char* args;
    uchar left_arity;
    size_t curr_bytes;
};

struct arg2 {
    char x1;
    float x2;
};

struct add_args {
    int x1;
    int* x2;
};

/*

f0 : int -> int -> int 
x  : int 

f1 : int -> int = f0 x 

f2_temp : int -> int 

f2 x : int -> int -> int = f1

f3 : int -> int = f2 x

let getAdder x = 
    let adder x y z = 
        let innerAdder x y z w q = x + y + z + w + q
        in innerAdder x y z 
    in adder x 

let getAdder x : int -> (int -> int -> int -> int -> int) = 
    let adder x y z : int -> int -> int -> (int -> int -> int) = 
        let innerAdder x y z w q : int -> int -> int -> int -> int -> int == 
            x + y + z + w + q
        innerAdder x y z 
    adder x 
*/

int pre_innerAdder(char* data) {
    int* args = (int*)data;
    return innerAdder(args[0], args[1], args[2], args[3], args[4]);
}

int innerAdder(int x, int y, int z, int w, int q) {
    return x + y + z + w + q;
}

struct thunk adder(int x, int y, int z) {
    struct thunk th;
    
    th.fn = &pre_innerAdder;
    th.left_arity = 2;
    th.args = malloc(sizeof(int) * 3);

    int data[] = {x, y, z};

    memcpy(th.args, data, sizeof(int) * 3);
    return th;;
}

struct thunk pre_adder(char* data) {
    int* args = (int*)data;
    return adder(args[0], args[1], args[2]);
}

struct thunk getAdder(int x) {
    struct thunk th;
    th.fn = &pre_adder;
    th.left_arity = 2;
    th.args = malloc(sizeof(int));
    th.curr_bytes = sizeof(int);
    memcpy(th.args, &x, sizeof(int));

    return th;
}

struct thunk pre_getAdder(char* data) {
    return getAdder(((int*)data)[0]);
}

char* generic_apply(struct thunk* th, uchar cnt, char* args, size_t args_size) {
    char* final_args = malloc(th->curr_bytes + args_size);

    // copy all required bytes to one array
    memcpy(final_args, th->args, th->curr_bytes);
    memcpy(final_args + th->curr_bytes, args, args_size);

    if (th->left_arity == cnt) {
        // pass thunk with all args (as byte array) to th->fn
        return th->fn(final_args);
    }
    else {
        struct thunk* res = malloc(sizeof(struct thunk));
        res->left_arity = th->left_arity - cnt;
        res->args = final_args;
        res->fn = th->fn;
        res->curr_bytes = th->curr_bytes + args_size;

        return (char*)res;
    }
}

struct thunk g;

int rrrr = 1;

void test() {
    struct thunk th = getAdder(1<<20);
    th;

    int next_arg = 2;
    char* res1 = generic_apply(&th, 1, (char*)&next_arg, sizeof(int));
    res1;

    rrrr ^= (int)&th;
    rrrr ^= (int)res1;

    struct thunk th2;
    memcpy(&th2, res1, sizeof(th2));
    g = th2;
    rrrr ^= (int)&g;
}

int x;

int fast_add(int a, int b, int c, int d) {
    return a + b + c + d;
}

// void* apply2 (void* (*fn)(void*, void*), void** args) {
//     return fn(args[0], args[1]);
// }

// void* some_apply(void* some, void** args) {
//     void* (*fn)(void*, void*) = some;
//     apply2(fn, args);
// }

char* retArr() {
    char tmp[12] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
    char* res = malloc(12);
    memcpy(res, tmp, 12);
    return res;
}

int main() {
    test();

    // char* res = retArr();

    // for (int i = 0; i < 12; i++) {
    //     printf("%d ", (int)res[i]);
    // }
    // puts("");

    // int n = 1000000000;
    
    // clock_t t;
    // t = clock();
    
    // for (int i = 0; i < n; i++)
    //     x ^= fast_add(i, i - 100, i * i, i * 100 + (i - 100 * i));
    
    // t = clock() - t;
    // double time_taken = ((double)t)/CLOCKS_PER_SEC;
    // printf("Elapsed time: %.3lf\n", time_taken);

    // t = clock();
    
    // for (int i = 0; i < n; i++)
    //     test ();
    // printf("%d", rrrr);

    // t = clock() - t;
    // time_taken = ((double)t)/CLOCKS_PER_SEC;
    // printf("Elapsed time: %.3lf\n", time_taken);

}