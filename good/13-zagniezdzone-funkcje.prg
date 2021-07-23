// Wypisze 5
int zewnetrzna(int x) {
  if (x >= 5) return x;
	int wewnetrzna(int y) {
		y++;
		return zewnetrzna(y);
	}
  return wewnetrzna(x);
}
print(zewnetrzna(1));
