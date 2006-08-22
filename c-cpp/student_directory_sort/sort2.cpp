#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

struct student{
   string name, year, phone, room;
   int msc;
};

void print_student(student* stu, int output_all = 1) {
   cout << "name:" << stu->name << " year:" << stu->year << " msc:" << stu->msc
         << " phone:" << stu->phone << " room:" << stu->room << endl;
}

student* parse_student(string stu) {
   student* kid = new student;
   kid->name = stu.substr(0, 33);
   kid->year = stu.substr(34, 5);
   kid->msc = atoi(stu.substr(40, 4).c_str());
   kid->phone = stu.substr(45,14);
   kid->room = stu.substr(59);
   return kid;
}
   
int partition(vector<student*> &a, int lft, int rgt) {
   int i = lft-1;
   int j = rgt;
   int v = a[rgt]->msc;
   while(1) {
      while(a[++i]->msc < v);
      while(v < a[--j]->msc) if (j == lft) break;
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

void output_students(vector<student*> &array) {
   unsigned int p1 = 0;
   unsigned int p2 = array.size()/2+1;
   ofstream fout("output.dat");
   while(p2 < array.size()) {
      fout << array[p1]->msc << "  " << array[p1]->name << " "
         << array[p2]->msc << "  " << array[p2]->name << endl;
      p2 +=1; p1 += 1;
   }
   if(p1 < array.size()/2+1) { 
      fout << array[p1]->msc << "  " << array[p1]->name << endl;
   }
}   

int main(void) {
   ifstream fin("fall.prn");
   char line[1056];
   student *kid;
   vector<student*> sort;

   while(fin.getline(line, 1056)){
      kid = parse_student(string(line));
      sort.push_back(kid);
   }

   qsort_students(sort, 0, sort.size()-1);
   output_students(sort);
	
   return 0;
}
