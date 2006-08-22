#include <iostream>
#include <stdlib.h>
#include "settype.h"

const int SIZE = 100;

int main(int argc, char *argv[])
{
  /* sets to be used for testing */
  char s[5] = {'1','2','3','4','\0'};
  char t[4] = {'1','2','3','\0'};
  char u[1] = {'\0'};
  char v[4] = {'x','y','z','\0'};
  
  /* sets to contain results */
  char a[SIZE];
  char b[SIZE];
  char c[SIZE];
  char d[SIZE];
  
  /* SetTypes for testing */
  SetType set1(s);
  SetType set2(t);
  SetType set3(u);
  
  /* test of is_empty. Should output "Set is empty" followed by "Set is not
      empty */
  set3.is_empty() ? cout << "Set is empty\n" : cout << "Set is not empty\n";
  set1.is_empty() ? cout << "Set is empty\n" : cout << "Set is not empty\n";
  
  /* test of is_equal. Should output "s is equal" followed by "t is not equal" 
  */
  set1.is_equal(s) ? cout << "s is equal\n" : cout << "s is not equal\n";
  set1.is_equal(t) ? cout << "t is equal\n" : cout << "t is not equal\n";
  
  /* test of is_member. Should output "4 is a member" followed by "8 is not a
      member */
  set1.is_member('4') ? cout << "4 is a member\n" : cout << "4 is not a member\n";
  set1.is_member('8') ? cout << "8 is a member\n" : cout << "8 is not a member\n";
  
  /* test of is_subset. Should output "t is a subset" followed by "v is not a
      subset */
  set1.is_subset(t) ? cout << "t is a subset\n" : cout << "t is not a subset\n";
  set1.is_subset(v) ? cout << "v is a subset\n" : cout << "v is not a subset\n";
  
  /* test of setunion. Should output all elements from s and v. Implicitly
      tests the write function. */
  set1.setunion(v, a);
  SetType set4(a);
  set4.write();
  
  /* test of intersection. Should output all elements in both t and s. */
  set1.intersection(t, b);
  SetType set5(b);
  set5.write();
  
  /* test of difference. Should output all elements only in t or only in s */
  set1.difference(t, d);
  SetType set7(d);
  set7.write();

  /* return success */
  return 0;
}
