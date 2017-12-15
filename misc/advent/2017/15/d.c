/* clang d.c -O3 -o d */
/* This is .3s, pypy is only .42! */
#include<stdio.h>

int go(long a, long b, long iters) {
    int judge = 0;
    for (int i=0; i < iters; i++) {
        while(1) {
            a = a * 16807 % 2147483647;
            if (a % 4 == 0) break;
        }
        while(1) {
            b = b * 48271 % 2147483647;
            if (b % 8 == 0) break;
        }
        if ((a & 0xFFFF) == (b & 0xFFFF))
            judge += 1;
    }
    return judge;
}

int main() {
    printf("%d\n", go(783, 325, 5000000));
    return 0;
}
