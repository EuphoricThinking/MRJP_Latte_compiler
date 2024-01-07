#include <stdio.h>
#include <stdlib.h>

// gcc runtime.c -c -o ../lib/runtime.o

void printInt(int a) {
    printf("%d\n", a);
}

void error() {
    fprintf(stderr, "runtime error\n");
    exit(EXIT_FAILURE);
}

void printString(char* c) {
    printf("%s\n", c);
}

int readInt() {
    int res;
    scanf("%d", &res);
    getchar(); // consumes trailing newline from buffer

    return res;
}

// char* createString(char* toCopy) {
//     return strdup(toCopy);
// }

char* readString() {
    char *line = NULL;
    size_t len = 0;
    size_t read = getline(&line, &len, stdin);
    line[read - 1] = '\0';

    return line;
}