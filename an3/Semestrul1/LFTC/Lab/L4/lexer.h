// lexer.h
#ifndef LEXER_H
#define LEXER_H

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int capacity;
    int size;
    char** table;
} TS;

void initHashTable(TS* ts, int capacity);
void printFIP();
void printTS(TS* idsHashTable, TS* constsHashTable);
void freeHashTable(TS* hashTable);

extern TS idsHashTable;
extern TS constsHashTable;

extern int hasErrors;
extern int lineNum;

#endif // LEXER_H
