/*
Bill Mill && Bryan Brook
CS 371
MSC Sorting program - quicksort, mergesort
File Name: student.h
*/

/*
This file defines a student object and several methods for the object.
I put the implementation in with the class definition because it is so 
small.
*/

#ifndef __STUDENT_H  //don't include this file twice
#define __STUDENT_H

#include <string>    //using strings
#include <iostream>  //and cout, cin
using namespace std; //I *still* don't like typing std::

class student {
   private:
      string name, year, phone, room;   //student data
      int msc;                          //student msc#

   public:
      student() { }                     //constructor

      /* 
      parse a line from the prn file into a student. The string should
      be a line from the prn file I created for the class.
      */
      void parse_student(string stu) {
         name = stu.substr(0, 33);              //read name
         year = stu.substr(34, 5);              //read year
         msc = atoi(stu.substr(40, 4).c_str()); //convert msc from a string
                                                //into an int
         phone = stu.substr(45,14);             //read phone #
         room = stu.substr(59);                 //read room
      }
      
      /*
      msc accessor function
      */
      int get_msc() { return msc; }
      
      /*
      name accessor function
      */
      string get_name() { return name; }
};

#endif
