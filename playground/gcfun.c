// #define GC_DEBUG 1

#include <stdio.h>
#include <gc.h>

struct lst {
    struct lst* next;
    long val;
};

void other_fun() {
    int cnt = 1000000;

    struct lst* last = NULL;;

    while (cnt--) {
        // puts("PRE MALLOC int");
        struct lst* mutint = (struct lst*) GC_MALLOC(sizeof(struct lst));
    
        // puts("MALLOC int");
        mutint->next = (struct lst*) GC_MALLOC(sizeof(struct lst));
        mutint->val = cnt + 10;

        last = mutint;

        // printf("ptr: %zu\nval:%lld\nnext ptr: %zu\n", (size_t)mutint, *mutint, mutint->next);
    }
    printf("ptr: %zu\nval:%lld\nnext ptr: %zu\n", (size_t)last, *last, (size_t)last->next);
}

int main() {
    GC_free_space_divisor = 1;
    GC_init();

    other_fun();
    GC_gcollect();
}