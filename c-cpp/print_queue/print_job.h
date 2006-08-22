#ifndef __PRINT_JOB_H
#define __PRINT_JOB_H

#include <string>
#include <Mersenne/MersenneTwister.h>
using namespace std;

enum priority_level {NORMAL, HIGH, URGENT};

class print_job {
   public:
      priority_level priority;
      int length;
      int cycles;
      int time;
      print_job(int t);
   private:
      MTRand random;
};

bool operator< (const print_job &a, const print_job&b);

#endif
