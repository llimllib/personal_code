#include <iostream>
#include <fstream>
#include <string>
#include <deque>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#define NUM_THREADS     1
#define BUF_SIZE        128         //max size of our buffer
using namespace std;

deque<string> buf2;
sem_t buf_lock, empty, full;

void *Producer(void *threadid)
{
    string in;
    ifstream fin("f1");
    int count;
    fin >> in;
    while(!fin.eof()) {
        sem_wait(&empty);
        sem_wait(&buf_lock);
        sem_getvalue(&empty, &count);
        cout << count << "\n";
        buf2.push_back(in);
        sem_post(&buf_lock);
        sem_post(&full);
        fin >> in;
        sleep(rand() % 2);
    }
    sem_wait(&empty);
    sem_wait(&buf_lock);
    buf2.push_back("<<<EOF>>>");
    sem_post(&buf_lock);
    sem_post(&full);
    fin.close();
    pthread_exit(0);
}

void *Consumer(void *threadid)
{
    string out;
    ofstream fout("f2");
    while(1) {
        sem_wait(&full);
        sem_wait(&buf_lock);
        out = buf2.front();
        buf2.pop_front();
        sem_post(&buf_lock);
        sem_post(&empty);
        if(out == "<<<EOF>>>") break;
        else fout << out << "\n";
        sleep(rand() % 3);
    }
    fout.close();
    pthread_exit(0);
}

int main (int argc, char *argv[])
{
   pthread_t threads[NUM_THREADS];
   int rc;

   sem_init(&buf_lock, 0, 1);
   sem_init(&empty, 0, BUF_SIZE);
   sem_init(&full, 0, 0);
   cout<<"Creating Producer\n";
   rc = pthread_create(&threads[0], NULL, Producer, NULL);
   if (rc){
     cout<<"ERROR; return code from pthread_create() is "<<rc<<"\n";
     return(-1);
   }
   cout<<"Creating Consumer\n";
   rc = pthread_create(&threads[1], NULL, Consumer, NULL);
   if (rc){
     cout<<"ERROR; return code from pthread_create() is "<<rc<<"\n";
     return(-1);
   }
   pthread_exit(NULL);
}
