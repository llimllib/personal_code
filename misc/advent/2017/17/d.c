#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct List {
    int data;
    struct List* next;
};
typedef struct List* Node;

const uint32_t SIZE = 50000001;
Node BUF;

void printlist() {
    printf("%d ", BUF->data);
    Node tmp = BUF->next;
    while(tmp != BUF) {
        printf("%d ", tmp->data);
        tmp = tmp->next;
    }
    printf("\n");
}

void run(int mod, int iters) {
    uint32_t steps = 1;
    BUF = malloc(sizeof(struct List));
    BUF->data = 0;
    BUF->next = malloc(sizeof(struct List));
    BUF->next->data = 1;
    BUF->next->next = BUF;
    Node ptr = BUF->next;
    Node tmp;
    for (int i=2; i < iters+1; i++) {
        for (int j=0; j < mod; j++) {
            ptr = ptr->next;
        }
        tmp = ptr->next;
        ptr->next = malloc(sizeof(struct List));
        ptr->next->data = i;
        ptr->next->next = tmp;
        ptr = ptr->next;
    }
    tmp = BUF;
    do {
        if (tmp->data == 0) {
            printf("%d %d\n", tmp->data, tmp->next->data);
            return;
        }
        tmp = tmp->next;
    } while(tmp != BUF);
    printf("uh oh");
}

int main() {
    run(3, 2017);
    run(312, 2017);
    /* takes about 8 minutes */
    run(312, 50000000);
    exit(0);
}
