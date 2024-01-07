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

int readInt() {
    int res;
    scanf("%d", &res);
    getchar(); // consumes trailing newline from buffer

    return res;
}