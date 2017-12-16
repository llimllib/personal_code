#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap(char chars[], int a, int b) {
    char tmp = chars[a];
    chars[a] = chars[b];
    chars[b] = tmp;
}

typedef struct Command {
    char* cmd;
    int a;
    int b;
} Command;

Command* parsecmd(char* line) {
    char *parts, *as, *bs;
    Command* cmd = malloc(sizeof(Command));

    parts = strtok(line, " ");
    cmd->cmd = parts;
    parts = strtok(NULL, " ");
    as = parts;
    if (as != NULL) {
        parts = strtok(NULL, " ");
        bs = parts;
    }
    if (strcmp(cmd->cmd, "s") == 0) {
        cmd->a = atoi(as);
    } else if (strcmp(cmd->cmd, "x") == 0) {
        cmd->a = atoi(as);
        cmd->b = atoi(bs);
    } else if (strcmp(cmd->cmd, "p") == 0) {
        cmd->a = (int)as[0];
        cmd->b = (int)bs[0];
    }
    return cmd;
}

char* run(Command* commands[], int cycles) {
    char progs[17] = "abcdefghijklmnop";
    char temp[16];
    char* ac;
    char* bc;

    for (int c=0; c < cycles; c++) {
        for (int i=0; i < 10000; i++) {
            Command *cmd = commands[i];
            //printf("%s %d %d\n", cmd->cmd, cmd->a, cmd->b);
            if (strcmp(cmd->cmd, "s") == 0) {
                for (int k=0; k < 16; k++) {
                    temp[k] = progs[(16-cmd->a+k) % 16];
                }
                for (int k=0; k < 16; k++) {
                    progs[k] = temp[k];
                }
            } else if (strcmp(cmd->cmd, "x") == 0) {
                temp[0] = progs[cmd->a];
                progs[cmd->a] = progs[cmd->b];
                progs[cmd->b] = temp[0];
            } else if (strcmp(cmd->cmd, "p") == 0) {
                ac = strchr(progs, cmd->a);
                bc = strchr(progs, cmd->b);
                temp[0] = progs[ac-progs];
                progs[ac-progs] = progs[bc-progs];
                progs[bc-progs] = temp[0];
            }
        }
        //printf("cycle %d\n", c);
    }

    return strdup((char*)progs);
}

int main()
{
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;
    Command* buf[10000];
    int i = 0;

    fp = fopen("inputp.txt", "r");
    if (fp == NULL)
        exit(1);

    while ((read = getline(&line, &len, fp)) != -1) {
        buf[i] = parsecmd(strdup(line));
        i += 1;
    }
    //printf("%s\n", run(buf, 1));
    //printf("%s\n", run(buf, 1000));
    printf("%s\n", run(buf, 1000000));

    fclose(fp);
    if (line)
        free(line);
    exit(0);
}
