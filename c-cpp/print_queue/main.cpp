#include <iostream>
#include <vector>
#include <Mersenne/MersenneTwister.h>
#include "print_job.h"
#include "printer.h"
using namespace std;

int main(void) {
   print_job *pj;
   MTRand random;                    /* random number generator */
   printer p;            /* a printer object */
   int days = 500;                     /* how many days to simulate */
   int jobs = 2000;                   /* jobs per 1st 8 hours of day */
   int jobs_done;                  /* inc. if all jobs done at end of day */
   int done_early;                 /* inc. if all jobs done after 9 hrs. */
   int delta = jobs/2;
   float prob = (float)jobs / (p.cycles_ph * 8);/* probability of a job in hrs 1-8 */
   float prob9 = prob / 2;             /* probability of a job in hr 9 */
   float prob10 = prob / 4;            /* probability of a job in hr 10 */
   float r;                            /* number returned by rand() */
   bool done = false;                  /* have we met the conditions? */

   while(!done) {
      cout << "trying " << jobs << " jobs" <<endl;
      jobs_done = 0;
      done_early = 0;
      /* simulate days */
      for(int i = 0; i < days; i++) {
         /* simulate each hr */
         for(int hr = 0; hr < 10; hr++) {
            if(hr == 9 && p.jobs_left() == 0) done_early++;
         /*simulate each cycle */
         for(int c = 0; c < p.cycles_ph; c++) {
            r = random.rand();         /* 0 <= r <= 1 */
            if(hr < 8 && r < prob) {
               pj = new print_job((hr * p.cycles_ph)+c);
               p.print(pj);
            }
            else if(hr < 9 && r < prob9) {
               pj = new print_job((hr * p.cycles_ph)+c);
               p.print_tomorrow(pj);
            }
            else if(hr < 10 && r < prob10) {
               pj = new print_job((hr * p.cycles_ph)+c);
               p.print_tomorrow(pj);
            }
            p.run_cycle();
         }} //cycles and hours
         if(p.jobs_left() == 0) { jobs_done++; }
         p.end_of_day();
         cout << i << endl;
      }//days
      cout << "jobs done: " << jobs_done << " done early: " << done_early << endl;
      if((float)jobs_done / days > .99 
         && (float)done_early / days > .95) {
         if(delta < 100) done = true;
         else {
            jobs = jobs + delta;
            delta /= 2;
         }
      }
      else {
         jobs = jobs - delta;
         delta /= 2;
      }
   }
   return 0;
}
