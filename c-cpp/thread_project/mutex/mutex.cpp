/* Thanks to 
http://www.llnl.gov/computing/tutorials/workshops/workshop/pthreads/MAIN.html
for the tutorial which I modified to make this program */

#include <iostream>
#include <pthread.h>
#define NUM_THREADS     2
using namespace std;

float accnt1 = 0, accnt2 = 0;
pthread_mutex_t accntlock = PTHREAD_MUTEX_INITIALIZER;


void *PrintHello(void *threadid)
{
    int counter = 0, id = (int)threadid;
    float r, sum;
    do {
        r = rand();
        pthread_mutex_lock(&accntlock);
        accnt1 += r;
        accnt2 -= r;
        sum = accnt1 + accnt2;
        pthread_mutex_unlock(&accntlock);
        counter++;
    } while (sum == 0 && counter < 10000000); //quit at 5 million
    cout<<"Thread #"<<id<<": "<<counter<<"\n";
    pthread_exit(0);
}

int main (int argc, char *argv[])
{
   pthread_t threads[NUM_THREADS];
   int rc, t;
   for(t=0;t < NUM_THREADS;t++){
      cout<<"Creating thread "<<t<<"\n";
      rc = pthread_create(&threads[t], NULL, PrintHello, (void *)t);
      if (rc){
         cout<<"ERROR; return code from pthread_create() is "<<rc<<"\n";
         return(-1);
      }
   }
   pthread_exit(NULL);
}
