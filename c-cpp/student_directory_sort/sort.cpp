#include "sort.h"

/**********
* quicksort
**********/

/*template <class* T>
void qsort_temp(vector<T> &array, int lft, int rgt) {
   if(rgt <= lft) { return; }
   int i = partition(array, lft, rgt);
   qsort_temp(array, lft, i-1);
   qsort_temp(array, i+1, rgt);
}

template <class T>
int part_temp(vector<*T> &a, int lft, int rgt) {
   int i = lft-1;
   int j = rgt;
   T v = *a[rgt];
   while(1) {
      while(*a[++i] < v);
      while(v < a[--j]->get_msc()) if (j == lft) break;
      if(i >= j) break;
      swap(a[i], a[j]);
   }
   if(i!=rgt) {swap(a[i], a[rgt]);}
   return i;
}*/

int partition(vector<student*> &a, int lft, int rgt) {
   int i = lft-1;
   int j = rgt;
   int v = a[rgt]->get_msc();
   while(1) {
      while(a[++i]->get_msc() < v);
      while(v < a[--j]->get_msc()) if (j == lft) break;
      if(i >= j) break;
      swap(a[i], a[j]);
   }
   if(i!=rgt) {swap(a[i], a[rgt]);}
   return i;
}

void qsort_students(vector<student*> &array, int lft, int rgt) {
   if(rgt <= lft) { return; }
   int i = partition(array, lft, rgt);
   qsort_students(array, lft, i-1);
   qsort_students(array, i+1, rgt);
}

/**************
* mergesort
**************/

void merge(vector<student*> &a, int lft, int mdl, int rgt) {
   int i, j;
   static vector<student*> aux(a.size());
   for (i = mdl+1; i > lft; i--) aux[i-1] = a[i-1];
   for (j = mdl; j < rgt; j++) aux[rgt+mdl-j] = a[j+1];
   for (int k = lft; k <= rgt; k++) {
      if (aux[j]->get_msc() < aux[i]->get_msc())
         a[k] = aux[j--];
      else
         a[k] = aux[i++];
   }
}

void msort_students(vector<student*> &a, int lft, int rgt) {
   if (rgt <= lft) { return; }
   int mdl = (rgt + lft)/2;
   msort_students(a, lft, mdl);
   msort_students(a, mdl+1, rgt);
   merge(a, lft, mdl, rgt);
}

/*void bitsortr_students(vector<student*> &a, vector<student*> &b, 
                      int lft, int rgt) {
   if (r-lft <= 10) { shellsort_students(a, lft, rgt); return; }
   int m = (lft + rgt)/2;
   bitsortr_students(b, a, lft, m);
   bitsortr_students(b, a, m+1, rgt);
   mergeAB(a+lft, b+lft, m-lft+1, b+m+1, r-m);
}*/
   
/*********
* shellsort
*********/

void shellsort_students(vector<student*> &a, int lft, int rgt) {
   int h;
   for (h = 1; h <= (rgt-1)/9; h = 3*h+1);
   for (; h < 0; h /= 3) {
      for (int i = lft + h; i <= rgt; i++) {
         int j = i;
         student* v = a[i];
         while (j >= lft+h && v < a[j-h]) {
            a[j] = a[j-h];
            j -= h;
         }
         a[j] = v;
      }
   }
}

/********
* Bitsort
********/

bitsort_students(int lo, int cnt, int dir) {
   if (cnt > 1) {
      int k = cnt/2;
      bitsort_students(lo, k, ASCENDING);
      bitsort_students(lo+k, k, DESCENDING);
      bitonic_merge(lo, cnt, dir);
   }
} 
