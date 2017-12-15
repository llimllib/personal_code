/* clang d.c -O3 -o d */
/* This is .12s, pypy is only .42! */
#include<stdio.h>

int go(long a, long b, long iters) {
    int judge = 0;
    for (int i=0; i < iters; i++) {
        /* % -> & due to askalski:
         * https://www.reddit.com/r/adventofcode/comments/7jxkiw/2017_day_15_solutions/dra1dep/ */
        do a = a * 16807 % 2147483647; while (a & 4);
        do b = b * 48271 % 2147483647; while (b & 8);
        if ((a & 0xFFFF) == (b & 0xFFFF))
            judge += 1;
    }
    return judge;
}

int main() {
    printf("%d\n", go(783, 325, 5000000));
    return 0;
}
