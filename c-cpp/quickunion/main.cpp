#include <iostream>
using namespace std;

static const int N = 10;

int main() {
   int i,j,p,q,id[N],sz[N];
   for (i=0; i < N; i++) { id[i] = i; sz[i] = 1;}
   while (cin >> p >> q) {
      for (i = p; i != id[i]; i = id[i]);
      for (j = q; j != id[j]; j = id[j]);
      if (i == j) continue;
      if (sz[i] < sz[j]) {id[i] = j; sz[j] += sz[i];}
      else {id[j] = i; sz[i] += sz[j];}
      cout << "\nindex: ";
      for (i=0; i < N; i++) { cout << i << " "; }
      cout << "\nid:    ";
      for (i=0; i < N; i++) { cout << id[i] << " "; }
      cout << "\nsize:  ";
      for (i=0; i < N; i++) { cout << sz[i] << " "; }
   }
}
