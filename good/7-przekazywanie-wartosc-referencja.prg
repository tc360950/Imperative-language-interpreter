void zamien(int x, int &y) {
  int tmp = x;
  x = y;
  y = tmp;
}

int a = 0;
int b = 1;
zamien(a , b);
print(a); //0
print(b); //0
