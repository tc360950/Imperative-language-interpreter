arr int[10][10] x;
int y = 0;
while (y < 100) {
  x[y/10][y % 10] = y;
  y++;
}

print(x[3][3]);//33

