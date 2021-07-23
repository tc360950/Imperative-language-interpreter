int funkcja(int x, fun int(int) funkcja2) {
    return x - funkcja2(x);
}

int fun2(int x) {
  return x*x;
}

print(funkcja(3, fun2));//-6
