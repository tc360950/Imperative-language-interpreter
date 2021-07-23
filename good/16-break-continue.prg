// wypisze 0 , 1, 2, 4, 5
int x = -1;
while (true) {  
  x++;
  if (x == 3) {
    continue;
  }
  if (x > 5)
    break;
  print(x);
}
