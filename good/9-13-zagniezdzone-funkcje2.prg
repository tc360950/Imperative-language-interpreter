int g(int x){
return x;
}

int fun1(int x) {

  int g(int x) {

    int g(int x) {

      int g(int x) {
        return 2*x;
      }

      return 3*g(x);
    }

    return 4*g(x);
  }

  return g(x);
}


print(fun1(1)); //wypisze 24
