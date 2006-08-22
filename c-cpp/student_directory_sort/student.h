#ifndef __STUDENT_H
#define __STUDENT_H

#include <string>
#include <iostream>
using namespace std;

class student {
   private:
      string name, year, phone, room;
      int msc;

   public:
      student() { }

      void print_student(void) {
            cout << "name:" << name << " year:" << year << " msc:" << msc
               << " phone:" << phone << " room:" << room << endl;
      }

      void parse_student(string stu) {
         name = stu.substr(0, 33);
         year = stu.substr(34, 5);
         msc = atoi(stu.substr(40, 4).c_str());
         phone = stu.substr(45,14);
         room = stu.substr(59);
      }
   
      int get_msc() { return msc; }
      string get_name() { return name; }
      bool operator <(student* stu) { cout << "here\n"; return msc < stu->get_msc(); }
};

#endif
