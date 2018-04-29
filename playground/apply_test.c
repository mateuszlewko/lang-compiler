#include <stdio.h>

typedef long long LL;

struct large {
    long arr[1000];
};

long callee(LL a, LL b, LL c, LL d, LL e, struct large la) {
    printf("%lld %lld %lld %lld %lld %ld\n", a, b, c, d, e, la.arr[700]);
    return a + b + c + d + e + la.arr[700];
}

long callee2(LL a, LL b, LL c, LL d, LL e, struct large la) {
    printf("%lld %lld %lld %lld %lld %ld\n", a, b, c, d, e, la.arr[900]);
    return a + b - c + d - e + la.arr[900];
}


int main() {
    LL a, b, c;
    scanf("%lld %lld %lld", &a, &b, &c);
    
    LL (*fn)(LL, LL, LL);

    if (a % 2 == 0)
        fn = callee;
    else fn = callee2;
    
    printf("%lld\n", fn(a, b, c));
}
