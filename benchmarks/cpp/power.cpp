#include <stdio.h>

int power(int a, int n) {
    if (n == 0)
        return 1;
    else return a * power(a, n - 1);
}

int main() {
    printf("%d\n", power(3, 200000));
}