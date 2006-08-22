#include <iostream>
#include <pthread.h>
using namespace std;

#define NUM_THREADS 2

float accnt1 = 0, accnt2 = 0;
pthread_mutex_t mutex_s = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t delay_s = PTHREAD_MUTEX_INITIALIZER;
int sem = 1;
int g_id = 0;

void P(int &s) {
    pthread_mutex_lock(&mutex_s);
    --s;
    if(s<0) {
        //pthread_cond_wait needs to reacquire &mutex_s before it
        //unblocks
        pthread_mutex_unlock(&mutex_s);
        pthread_mutex_lock(&delay_s);
    }
    pthread_mutex_unlock(&mutex_s);
}

void V(int &s) {
    pthread_mutex_lock(&mutex_s);
    if(++s <= 0) pthread_mutex_unlock(&delay_s);
    //Doesn't Work:
    //else pthread_mutex_unlock(&mutex_s);

    //Does Work:
    pthread_mutex_unlock(&mutex_s);
}   

void *PrintHello(void *threadid)
{
    int counter = 0, id = (int)threadid;
    float r, sum;
    do {
        r = rand();
        P(sem);
        accnt1 += r;
        accnt2 -= r;
        sum = accnt1 + accnt2;
        if(id != g_id) {
            printf("thread %d is now running\n", id);
            g_id = id;
        }
        V(sem);
    } while (sum == 0 && ++counter < 1000000); //quit at 10 million
    cout<<"Thread #"<<id<<": "<<counter<<"\n";
    pthread_exit(0);
}

int main(void) {
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
