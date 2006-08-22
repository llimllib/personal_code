/*
Bill Mill && Bryan Brook
CS 371
MSC Sorting program - quicksort, mergesort
File Name: sort.h
*/

/*
this file defines the interface to my sorting library. Only qsort_students,
msort_students, and shellsort_students should be called by external files.
I really should reimpliment this as a class, and I would if it wasn't due
in 7 hours.

By the way, in my testing (which has been deleted here because it only works
on my Linux machine) over 10000 iterations of the sort, shellsort took about
.003 seconds, quicksort 3 seconds, and mergesort 5 seconds. I suspect that
shellsort is the fastest because the data is partially ordered to start with.
*/

#ifndef __SORT_CPP   //Isn't it really dumb that C++ can't automatically
#define __SORT_CPP   //prevent you from including a file twice, forcing you
                     //to resort to C macro hacks?
#include <string>    //I use C++ strings
#include <vector>    //and C++ vectors
#include "student.h" //and call student functions
using namespace std; //I don't like to type std::

/* these functions are described in sort.cpp */

/*public functions*/
void qsort_students(vector<student*> &array, int lft, int rgt);
void msort_students(vector<student*> &a, int lft, int rgt);
void shellsort_students(vector<student*> &a, int lft, int rgt);

/*private functions*/
int partition(vector<student*> &a, int lft, int rgt);
void merge(vector<student*> &a, int lft, int mdl, int rgt);

#endif                //end C macro hack
