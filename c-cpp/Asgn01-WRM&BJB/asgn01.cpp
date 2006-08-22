/*
Bill Mill && Bryan Brook
CS 371
MSC Sorting program - quicksort, mergesort
File Name: main.cpp
*/

/*
This file contains the main loop of my program. It reads in a file called
fall.prn, creates a student object from each line, then sorts it by
various methods. Finally, it prints out the data nicely.

To compile, simply click file->open workspace and select ASGN01.dsw instead
of creating a default workspace. You may then compile and execute the
program.
*/

#include <iostream>        //standard I/O
#include <fstream>         //file I/O
#include <string>          //strings
#include <vector>          //vectors
#include "student.h"       //the student class
#include "sort.h"          //the sorting functions
using namespace std;       //I don't like typing std::

/*
nicely print out a vector of students into two columns.

a: any vector of student* objects
filename: the name of the file to print to
*/
void output_students(vector<student*> a, string filename) {
   ofstream fout(filename.c_str());    //output stream declaration
   unsigned int col2 = a.size()/2+1;   //the index of the first student
                                       //on the second column
   unsigned int col1 = 1;              //the number 1
   while (col2 < a.size()) {           //while there are students
      fout << a[col1]->get_msc() << " " //output the first msc
         << a[col1]->get_name()         //and the first name
         << a[col2]->get_msc() << " "   //then the second msc
         << a[col2]->get_name() << endl;//then the second name
      col1 += 1; col2 += 1;            //increment col1 and col2
   }
   if (col1 < a.size()/2+1) {          //if there's still a person left,
      fout << a[col1]->get_msc() <<    //print them out
         " " << a[col1]->get_name() << endl;
   }
}

int main(void) {
   ifstream fin("fall.prn");           //initialize the input stream
   char line[1056];                    //one line of fall.prn
   student* kid = new student;         //allocate space for a student
   vector<student*> sort;              //store sorted pointers
   vector<student*> unsorted;          //store unsorted pointers

   while(fin.getline(line, 1056)){     //read a line into, well, line
      kid->parse_student(string(line));//parse the line
      unsorted.push_back(kid);         //put a pointer into the unsorted vector
      kid = new student;               //allocate a new student
   }
   
   sort = unsorted;                    //prepare sort

   qsort_students(sort, 0, sort.size()-1);//quicksort students
   output_students(sort, "qsort.out"); //output results
   sort = unsorted;                    //restore the unsorted vector

   msort_students(sort, 0, sort.size()-1);//mergesort students
   output_students(sort, "msort.out"); //output results
   sort = unsorted;                    //restore the unsorted vector

   shellsort_students(sort, 0, sort.size()-1);//shellsort students
   output_students(sort, "ssort.out"); //output results

   return 0;
}
