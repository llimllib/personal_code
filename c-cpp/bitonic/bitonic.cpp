#include <iostream.h>
#include <iomanip.h>

void BitonicSort(int a[], int n);
void Swap(int &a, int &b);
void Spy(int a[], int n);
const int MAX=32;

int main(void) {
   int a[MAX]={9,26,25,3,5,19,10,17,20,13,24,22,12,6,31,7,8,11,2,15,16,14,28,29,33,32,27,4,21,23,18,30};
   cout << "unsorted list:\n";
   Spy(a,MAX);

   BitonicSort(a, MAX);

   cout << "sorted list:\n";
   Spy(a,MAX);
   return 0;
}

void BitonicSort(int a[], int n) {
   int i,j,k,ixj;
   for (k=2; k <= n; k = 2*k) {
      for(j=k>>1; j>0; j=j>>1) {
         for (i =0; i < n; i++) {
            ixj = i ^ j;
            cout <<"ixj: " << ixj << " " << i << " " << j << endl;
            if(ixj > i) {
               int z = i&k;
               cout << "i&k: "<<z<<" i:"<<i<<" k:"<<k
                  << " a[i]:"<<a[i]<<" a[ixj]:"<<a[ixj]<<"\n\n";
               if((i&k) == 0 && a[i] > a[ixj])
                  Swap(a[i], a[ixj]);

               if((i&k)!=0 && a[i] < a[ixj])
                  Swap(a[i], a[ixj]);
            }
         }
         cout << "j = " << j;
      }
      cout << endl << "k = "<<setw(3)<<k<<endl<<"a[]= ";
      Spy(a, MAX);
   }
}

void Swap(int &a, int &b) {
    int temp=a;
    a=b;
    b=temp;
}

void Spy(int a[], int n) {
    for(int i = 0; i < n; i++)
        cout << setw(4) << a[i];
    cout << "\n\n";
}
