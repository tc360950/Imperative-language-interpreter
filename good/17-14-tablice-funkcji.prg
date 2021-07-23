arr fun int(int) [4] x;
int fun1(int x){
return x+1;
}

int fun2(int x) {
  return x*x;
}

x[0] = fun1; x[1] = fun2;

fun int (int) f = x[1];
print(f(2));//4
