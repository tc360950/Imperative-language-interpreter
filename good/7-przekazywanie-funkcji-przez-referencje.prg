void fun1(int x, int y, fun int(int, int) &fun2) {
    int wewnetrzna(int u, int w) {
        return x*u + y*w;
     }
    fun2 = wewnetrzna;
}

int plus(int x, int y) {
    return x + y;
}

fun int(int, int) zmiennaFunkcja = plus;
// wypisze 3
print(zmiennaFunkcja(1, 2));

fun1(2, 3, zmiennaFunkcja);

// wypisze 8 
print(zmiennaFunkcja(1, 2));
