void* twice(void* (*f)(void*), void* x){
  return f(x);
}

int main() {return 0;}
