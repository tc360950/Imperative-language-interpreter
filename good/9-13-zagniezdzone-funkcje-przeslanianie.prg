string x = "global";
fun void() funkcja() {
  print(x);
  string x = "local";
  void fun2() {
    print(x);    
  }
  return fun2;
}


fun void() fun2 = funkcja();
fun2();
