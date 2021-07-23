int x = 10;

void funkcja() {
  print(x);
}

void funkcja2() {
  int x = 0;
  print(x);
}

void funkcja3() {
  boolean x = false;
  print(x);
}


{
  int x = -1;
  print(x);
  funkcja();
  funkcja2();
  funkcja3();
}
