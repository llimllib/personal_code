#ifndef __SORT_CPP
#define __SORT_CPP

#include <string>
#include <vector>
#include "student.h"
using namespace std;

int partition(vector<student*> &a, int lft, int rgt);
void qsort_students(vector<student*> &array, int lft, int rgt);

void merge(vector<student*> &a, int lft, int mdl, int rgt);
void msort_students(vector<student*> &a, int lft, int rgt);

void shellsort_students(vector<student*> &a, int lft, int rgt);

void bitsort_students(vector<student*> &a, int lo, int cnt);
#endif
