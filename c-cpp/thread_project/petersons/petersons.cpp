/*******************************
* This works but is tremendously slow.
* Thanks to
* http://www.cs.wvu.edu/~jdm/classes/cs356/notes/mutex/Peterson.html 
* for a slightly different algorithm than the one in the book.
********************************/
#include <iostream>
#include <pthread.h>
#define NUM_THREADS     2
using namespace std;

float accnt1 = 0, accnt2 = 0;
bool busy[NUM_THREADS] = {false, false}; //will only work for 2 threads as
                                         //written here
int turn = 0;

void *PrintHello(void *threadid)
{
    int counter = 0, id = (int)threadid;
    int otherid;
    float r;
    id == 0 ? otherid = 1 : otherid = 0;
    do {
        busy[id] = true;
        turn = otherid;
        while(busy[otherid] && turn != id) ;
        //I needed this debugging code to assure myself that it was working
        //at all
        //if(counter%100 == 0) 
        //    cout<<"working "<<id<<" at counter "<<counter<<"\n";
        r = rand();
        accnt1 += r;
        accnt2 -= r;
        counter++;
        busy[id] = false;
    } while (accnt1 + accnt2 == 0 && counter < 100000); //quit at 10000
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
