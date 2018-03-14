#include <iostream>
#include <functional>

int innerAdder(int x, int y, int z, int w, int q) {
    return x + y + z + w + q;
}

struct Str {
    long param;
    int arr[6];
    bool test;
}; 

// let getAdder x = 
//     printf "a"
//     let adder x y z = 
//         printf "b"
//         let innerAdder x y z w q = x + y + z + w + q
//         in innerAdder x y z 
//     in adder x 

// std::function<int(int)> adder1(int x, int x1, int x2, int x3) {
//     return [=](int q) {
//         return innerAdder(x, x1, x2, x3, q);
//     };
// }

// std::function<int(int, int)> adder2(int x) {
//     return [=](int w, int q) {
//         return innerAdder(x, y, z, w, q);
//     };
// }

int xx = 199;

// auto do_smth(std::function<std::function<int(int)>(int)> fn, int x) {
//     return fn(x);
// }


struct inAdd5 {
    int x1, x2, x3, x4;
    
    int operator() (int x5) {
        return x1 + x2 + x3 + x4 + x5;
    }
};

struct inAdd5_2 {
    int x1, x2, x3;
    
    int operator() (int x4, int x5) {
        return x1 + x2 + x3 + x4 + x5;
    }
};
struct inAdd5_3 {
    int x1, x2;
    
    int operator() (int x3, int x4, int x5) {
        return x1 + x2 + x3 + x4 + x5;
    }
};

struct inAdd4 {
    int x1, x2, x3;

    inAdd5 operator() (int x4) {
        inAdd5 add5 = {x1, x2, x3, x4};
        return add5;
    }
};

struct inAdd4_2 {
    int x1, x2;

    inAdd5 operator() (int x3, int x4) {
        inAdd5 add5 = {x1, x2, x3, x4};
        return add5;
    }
};

struct inAdd3 {
    int x1, x2;

    inAdd4 operator() (int x3) {
        inAdd4 add4 = {x1, x2, x3};
        return add4;
    }
};

struct inAdd2 {
    int x1;

    inAdd3 operator() (int x2) {
        inAdd3 add3 = {x1, x2};
        return add3;
    }
};

struct inAdd1 {
    inAdd2 operator() (int x1) {
        printf("a");
        inAdd2 add2 = {x1};
        return add2;
    }
};

struct adder3 {
    int x1, x2;

    inAdd4 operator() (int x3) {
        inAdd4 a = {x1, x2, x3};
        return a;
    }
};

struct adder2 {
    int x1;

    adder3 operator() (int x2) {
        adder3 a = {x1, x2};
        return a;
    }
};

struct adder1 {

    adder2 operator() (int x1) {
        adder2 a = {x1};
        return a;
    }
};

// auto nested_adder() {
//     return [=](int x) {
//         // xx ^= x / 2;
//         return [=](int y) {
//             return [=](int z) {
//                 // xx |= z;
//                 return [=](struct Str s2) {
//                     // xx &= w * 2;
//                     return [=](struct Str s) { 
//                         return innerAdder(x, y, z, s.arr[abs(x) % 4] + s.param, s2.test);
//                     };
//                 };
//             };
//         };
//     };
// }

int inAddTest(int x, int y, int z, int w, int q) {
    adder1 add1;
    // add1.operator()
    return add1(x)(y)(z)(w)(q);
}

void test() {
    // auto fn = nested_adder()(1)(4)(4)(5)(2);
    Str ssss;

    const int N = 100000000;
    for (int i = 0; i < N; i++) {
        // ssss.arr[2]=i;
        // xx ^= innerAdder(i, i * i, i - 100, -i, i*(9-i));
        xx ^= inAddTest(i, i * i, i - 100, -i, i*(9-i));
    }
}

// auto get3(int x, int y) {
//     return nested_adder()(x)(y);
// }

// void test2() {
//     const int N = 100000000;

//     for (int i = 0; i < N; i++) {
//         auto fn1 = get3(i * i, i);
//         auto fn2 = nested_adder()(-i)(i * i);
//         // xx ^= innerAdder(i, i * i, i - 100, -i, i*(9-i));
//         xx ^= fn1(i)(i * 10)(i);
//         xx |= fn2(i * i)(-i)(i / 4);
//     }
// }


int main() {
    // printf("%d\n", do_smth(nested_adder()(2)(3)(4), 4)(1));

    test();
    printf("%d\n", xx);
}
