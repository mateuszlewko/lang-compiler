#include <stdio.h>

void print1(int* arr) {
    printf("%d\n", *(arr+1));
}

int main()
{
    int arr[7] = {1, 2, 3, 4, 5, 6, 7};
    print1(arr);

    return 0;
}