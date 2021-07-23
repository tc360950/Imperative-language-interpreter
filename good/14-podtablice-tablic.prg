arr int[10][10] d;
arr int[10][10] x;

x = d;//x wskazuje na ten sam obszar pamieci co d

void funkcjaI(arr int[10] &y) {
y[0] = 333;
}


funkcjaI(d[2]);

print(d[2][0]);//333
print(x[2][0]);//333

arr int[10] y;
y = d[2];
y[1] = 12345;

print(d[2][1]);//12345
print(y[1]);//12345


{
arr int[10][10] xx;
arr int[10] y;

xx[1] = y;
y[5] = 12345;

print(xx[1][5]);//12345
}

