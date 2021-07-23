arr boolean[10] x;
x[0] = true;

void funkcja(arr boolean[10] &y) {
  y[0] = false;
}

print(x[0]);//true
funkcja(x);
print(x[0]);//false
