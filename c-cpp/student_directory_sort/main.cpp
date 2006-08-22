#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sys/time.h>
#include "student.h"
#include "sort.h"
using namespace std;

/* timers for the sort functions */
struct timeval t_start, t_stop;

void output_students(vector<student*> a, string filename) {
   ofstream fout(filename.c_str());
   uint col2 = a.size()/2+1;
   uint col1 = 1;
   while (col2 < a.size()) {
      fout << a[col1]->get_msc() << " " << a[col1]->get_name() 
         << a[col2]->get_msc() << " " << a[col2]->get_name() << endl;
      col1 += 1; col2 += 1;
   }
   if (col1 < a.size()/2+1) { 
      fout << a[col1]->get_msc() << " " << a[col1]->get_name() << endl;
   }
}

void start_clock() {
   gettimeofday(&t_start, NULL);
}

double stop_clock() {
   double diff;
   gettimeofday(&t_stop, NULL);
   diff = (double)t_stop.tv_sec + (double)t_stop.tv_usec * 1e-6;
   diff -= (double)t_start.tv_sec + (double)t_start.tv_usec * 1e-6;
   return diff;
}

int main(void) {
   ifstream fin("fall.prn");
   char line[1056];
   student* kid = new student;
   vector<student*> sort;
   vector<student*> unsorted;
   double qsort_t=0, msort_t=0, ssort_t=0;
   double n = 10000.0; //number of times to sort data

   while(fin.getline(line, 1056)){
      kid->parse_student(string(line));
      unsorted.push_back(kid);
      kid = new student;
   }
   
   sort = unsorted;

   for(int i = 0; i < n; i++) {
      sort = unsorted;
      start_clock();
      qsort_students(sort, 0, sort.size()-1);
      qsort_t += stop_clock();
   }

   for(int i = 0; i < n; i++) {
      sort = unsorted;
      start_clock();
      msort_students(sort, 0, sort.size()-1);
      msort_t += stop_clock();
   }

   for(int i = 0; i < n; i++) {
      sort = unsorted;
      start_clock();
      shellsort_students(sort, 0, sort.size()-1);
      ssort_t += stop_clock();
   }

   for( int i = 0; i < n; i++) {
      sort = unsroted;
      start_clock();
      shellsort_students(sort, 0, sort.size());
      ssort_t += stop_clock();
   }

   cout << "Recursive quicksort took " << qsort_t << " seconds.\n";
   cout << "Recursive mergesort took " << msort_t << " seconds.\n";
   cout << "Shellsort took " << ssort_t << " seconds.\n";
   output_students(sort, "ssort.out");

   return 0;
}
