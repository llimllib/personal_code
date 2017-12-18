#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const uint32_t SIZE = 50000001;
//const uint32_t SIZE = 5000001;
uint32_t BUF[SIZE];

void run(int mod, int iters) {
    //uint32_t buf[SIZE];
    uint32_t ptr = 1;
    BUF[0] = 0;
    BUF[1] = 1;
    for (int i=1; i < iters+1; i++) {
    //for (int i=2; i < 10; i++) {
        ptr = (ptr + mod) % i;
        //printf("<%d> ", ptr);
        for (int j=i; j > ptr; j--) {
            BUF[j] = BUF[j-1];
        }
        BUF[++ptr] = i;
        //for (int k=0; k < 10; k++) {
        //    printf("%d ", BUF[k]);
        //}
        //printf("\n");
    }
    for (int i=0; i < SIZE; i++) {
        if (BUF[i] ==  2017) {
            printf("%d %d %d\n", BUF[i-1], BUF[i], BUF[i+1]);
        }
    }
}

int main()
{
    run(3, 2017);
    run(312, 2017);
    run(312, 500000);
    //run(312, 5000000);
    exit(0);
}
