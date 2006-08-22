#include <iostream>
#include <semaphore.h>
using namespace std;

#define NUM_THREADS 2

float accnt1 = 0, accnt2 = 0;
sem_t sem;

void *PrintHello(void *threadid)
{
    int counter = 0, id = (int)threadid;
    float r, sum;
    do {
        r = rand();
        sem_wait(&sem);
        accnt1 += r;
        accnt2 -= r;
        sum = accnt1 + accnt2;
        sem_post(&sem);
        counter++;
    } while (sum == 0 && counter < 10000000); //quit at 10 million
    cout<<"Thread #"<<id<<": "<<counter<<"\n";
    pthread_exit(0);
}

int main(void) {
   pthread_t threads[NUM_THREADS];
   int rc, t;

   sem_init(&sem, 0, 1);
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
