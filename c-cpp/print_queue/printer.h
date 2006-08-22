#ifndef __PRINTER_H
#define __PRINTER_H

#include <queue>
#include "print_job.h"

class compare {
   public:
   bool operator()(const print_job *a, const print_job *b) {
      return *a < *b;
   }
};

/* necessary to allow ourselves to iterate over the priority_queue */
class print_queue : public priority_queue<print_job*, std::vector<print_job*>,
                                          compare> {
public:
  vector<print_job*>& all_jobs() { return c; }
};

class printer {
   public:
      int cycles_ph;                  /* cycles per hour */
      printer();
      void print(print_job *pj);
      void print_tomorrow(print_job *pj);
      void end_of_day();
      void run_cycle();
      int jobs_left();
   private:
      int pages_left;                       /* pages left on the current job */
      print_queue tomorrow_q; /* jobs to print tomorrow */
      print_queue print_q;    /* print queue */
};

#endif
