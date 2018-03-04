#include <iostream>

std::function<int(int, int)> getAdder (int x)
{
    auto adder = [&x](int a, int b) {
        return a + b + x;
    };

    return adder;
}

int main() {
    int x;
    scanf("%d", &x);
    
    auto adder = getAdder(2);
    printf("%d\n", adder(1, 2));
}