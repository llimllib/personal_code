#include "print_job.h"

print_job::print_job(int t) {
   time = t;
   
   /* generate random length according to the distribution given in the assignment */
   int l = random.randInt(99);
   if(l < 50) { length = 1; }
   else if (l < 70) { length = random.randInt(3) + 2; }
   else if (l < 85) { length = random.randInt(14) + 6; }
   else if (l < 95) { length = random.randInt(29) + 21; }
   else if (l < 98) { length = random.randInt(24) + 51; }
   else { length = random.randInt(24) + 76; }

   cycles = 2 + length;

   /* generate priority */
   if(length > 20) {priority = NORMAL;}//a length > 20 must be NORMAL
   else {
      int p = random.randInt(9);
      if(p < 7) { priority = HIGH; }   //70% of eligible jobs are HIGH
      else if(length < 6 && p == 9) {  //10% of eligible jobs are URGENT
         priority = URGENT;
      }
      else { priority = NORMAL; }      //the rest are NORMAL
   }
   
}

bool operator < (const print_job &a, const print_job &b) {
   if(a.priority < b.priority) {
      return true;
   }
   if(b.priority < a.priority) {
      return false;
   }
   if(a.time < b.time) {
      return true;
   }
   return false;
}
