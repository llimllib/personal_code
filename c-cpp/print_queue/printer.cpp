#include "printer.h"

printer::printer() {
   cycles_ph = 10000;
}

void printer::print(print_job *pj) {
   print_q.push(pj);
}

void printer::print_tomorrow(print_job *pj) {
   tomorrow_q.push(pj);
}

int printer::jobs_left() {
   return print_q.size();
}

//remember that initialization time was already added onto the print_job.length
void printer::run_cycle() {
   if (pages_left == 0) {
      if (!print_q.empty()) {
         pages_left = print_q.top()->length;
         delete print_q.top();
         print_q.pop();
      }
      else return;
   }
   else pages_left--;
}

void printer::end_of_day() {
   vector<print_job*> jobs = print_q.all_jobs();
   for(unsigned int i; i < jobs.size(); i++) {
      if(jobs[i]->priority == NORMAL) 
         jobs[i]->priority = HIGH;
      if(jobs[i]->priority == HIGH)
         jobs[i]->priority = URGENT;
   }
   while(!tomorrow_q.empty()) {
      print_q.push(tomorrow_q.top());
      tomorrow_q.pop();
   }
} 
