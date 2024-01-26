#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// gcc runtime.c -c -o ../lib/runtime.o
typedef struct arr {
    void* a;
    int len;
} arr;

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

char* ___concatenateStrings(char* s1, char* s2) {
    size_t l1 = strlen(s1);
    size_t l2 = strlen(s2);

    char* result = malloc(sizeof(char)*(l1 + l2 + 1));

    memcpy(result, s1, l1);
    memcpy(result + l1, s2, l2 + 1);

    return result;
}

arr* allocArray(size_t elemSize, int numElems) {
    arr* newArr = malloc(sizeof(arr));
    newArr->a = malloc(elemSize*numElems);
    newArr->len = numElems;

    return newArr;
}