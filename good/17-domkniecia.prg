fun void () zewn() {
  int x = 1;
  void wewnetrzna() {
    x++;
    print(x);
  }
  return wewnetrzna; 
}

fun void() funkcja = zewn();
funkcja();//2
funkcja();//3
