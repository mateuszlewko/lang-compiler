#include <iostream>
#include <stdio.h>
using namespace std;

int add_one(int n, int x, int c, int c2) {
    if (n == 0)
        return x;
    else
        return add_one(n - c2, x + c, c, c2);
}

int main() {
    int x, c, c2;
    scanf("%d %d %d", &x, &c, &c2);
    printf("%d\n", add_one(x, 0, c, c2));
}