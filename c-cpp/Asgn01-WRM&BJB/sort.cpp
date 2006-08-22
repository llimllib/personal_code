/*
Bill Mill && Bryan Brook
CS 371
MSC Sorting program - quicksort, mergesort
File Name: sort.cpp
*/

/*
This is my sorting library. It implements three different sorts of a vector
of student* objects. First is recursive quicksort, then recursive mergesort,
and finally a shellsort.
*/

#include "sort.h"

/**********
* quicksort
**********/

/*
Sort a vector of student* objects with the quicksort algorithm. The initial
values of lft and rgt that you supply should be the indices of the array
portion that you want sorted.
*/
void qsort_students(vector<student*> &array, int lft, int rgt) {
   if(rgt <= lft) { return; }             //if there's nothing left to divide,
                                          //return
   int i = partition(array, lft, rgt);    //find a dividing point in the vector
   qsort_students(array, lft, i-1);       //sort the left side
   qsort_students(array, i+1, rgt);       //sort the right side
}

/*
Find a spot to divide the vector. This function should only be used by the
qsort_students(...) function.
*/
int partition(vector<student*> &a, int lft, int rgt) {
   int i = lft-1;                         //
   int j = rgt;                           //
   int v = a[rgt]->get_msc();             //the value of the rightmost element
   while(1) {                             //do until break
      while(a[++i]->get_msc() < v);       //find an element, starting from the
                                          //left, which is greater than v. store
                                          //its value in i
      while(v < a[--j]->get_msc())        //find an element left of v whose value
                                          //is < v, store its index in j
         if (j == lft) break;             //make sure that we're in the vector
      if(i >= j) break;                   //stop if j reaches i
      swap(a[i], a[j]);                   //swap i and j because j < v < i
   }
   if(i!=rgt) {swap(a[i], a[rgt]);}       //if i!= rgt, swap them
   return i;                              //return i
}

/**************
* mergesort
**************/

/* 
note: I didn't comment these since I didn't have to do them. They do
work, I did them just because I wanted to. */

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
