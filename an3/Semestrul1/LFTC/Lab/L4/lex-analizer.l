%{

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "yacc-parser.tab.h"
#include "lexer.h"

unsigned int hash_function(int m, char element) {
    return (unsigned int)element % m;
}

void initHashTable(TS* ts, int capacity) {
    ts->capacity = capacity;
    ts->size = 0;
    ts->table = (char**)malloc(sizeof(char*) * capacity);

    for (int i = 0; i < capacity; i++) {
	ts->table[i] = NULL;
    }
}

int has_item(TS* hashTable, char* item) {
    unsigned int index = hash_function(hashTable->capacity, item[0]);
    if (hashTable->table[index] != NULL && strcmp(hashTable->table[index], item) == 0) {
        return index;
    }

    unsigned int newIndex = (index + 1) % hashTable->capacity;
    while (newIndex != index) {
        if (hashTable->table[newIndex] == NULL) {
            return -1;
        }
        if (strcmp(hashTable->table[newIndex], item) == 0) {
            return newIndex;
        }
        newIndex = (newIndex + 1) % hashTable->capacity;
    }

    return -1;
}

int add(TS* hashTable, char* item) {
    int i = has_item(hashTable, item);
    if(i != -1)
	return i; 
    unsigned int index = hash_function(hashTable->capacity, item[0]);
    if (hashTable->table[index] == NULL) {
        hashTable->table[index] = (char*)malloc(sizeof(char) * 280);
        strcpy(hashTable->table[index], item);
        hashTable->size++;
        return index;
    }

    unsigned int newIndex = (index + 1) % hashTable->capacity;
    while (newIndex != index) {
        if (hashTable->table[newIndex] == NULL) {
	    hashTable->table[newIndex] = (char*)malloc(sizeof(char) * 280);
	    strcpy(hashTable->table[newIndex], item);
            return newIndex;
        }
        newIndex = (newIndex + 1) % hashTable->capacity;
    }

    printf("Hash table is full\n");
    return -1;
}

TS idsHashTable;
TS constsHashTable;

typedef struct {
    int atomCode;
    int posTs;
}FIP;

FIP fip[500];
int fipLen = 0;

int hasErrors = 0;
int lineNum = 1;

void addToFIP(int atomCode, int posTs) {
    printf("%d\n", atomCode);
    fip[fipLen].atomCode = atomCode;
    fip[fipLen].posTs = posTs;
    fipLen++;
}

void printTS(TS* idsHashTable, TS* constsHashTable) {
    printf("\n===== TS-IDS =====\n\n");
    for (int i = 0; i < idsHashTable->capacity; i++) {
        if (idsHashTable->table[i] != NULL) {
            printf(" %s  %d\n", idsHashTable->table[i], i);
        }
    }
    printf("\n");

    printf("\n===== TS-CONST =====\n\n");
    for (int i = 0; i < constsHashTable->capacity; i++) {
        if (constsHashTable->table[i] != NULL) {
            printf(" %s  %d\n", constsHashTable->table[i], i);
        }
    }
    printf("\n");
}

void printFIP() {
    printf("\n===== FIP ======\n\n");
    for (int i = 0; i < fipLen; i++) {
	if (fip[i].posTs == -1) {
	    printf(" %d   .", fip[i].atomCode);
	} else {
	    printf(" %d   %d", fip[i].atomCode, fip[i].posTs);
	}
        printf("\n");
    }
    printf("\n");
}

void freeHashTable(TS* hashTable) {
    for (int i = 0; i < hashTable->capacity; i++) {
	free(hashTable->table[i]);
    }

    free(hashTable->table);
}

%}

%option noyywrap

digit	[0-9]
nndigit	[1-9]
letter	[a-zA-Z_]
number	{digit}|{nndigit}{digit}*
letterAndDigit	{digit}|{letter}

%%

"("			{addToFIP(2, -1); return LPAREN;}

")"			{addToFIP(3, -1); return RPAREN;}

","			{addToFIP(4, -1); return COMMA;}

"int"			{addToFIP(5, -1); return INT;}

"float"			{addToFIP(6, -1); return FLOAT;}

"string"		{addToFIP(7, -1); return STRING;}

"{"			{addToFIP(8, -1); return LBRACE;}

"}"			{addToFIP(9, -1); return RBRACE;}

";"			{addToFIP(10, -1); return SEMICOLON;}

"="			{addToFIP(11, -1); return ASSIGN;}

"cin"			{addToFIP(12, -1); return CIN;}

"cout"			{addToFIP(13, -1); return COUT;}

">>"			{addToFIP(14, -1); return RIGHT_SHIFT;}

"<<"			{addToFIP(15, -1); return LEFT_SHIFT;}

"+"			{addToFIP(16, -1); return PLUS;}

"-"			{addToFIP(17, -1); return MINUS;}

"*"			{addToFIP(18, -1); return MUL;}

"/"			{addToFIP(19, -1); return DIV;}

"%"			{addToFIP(20, -1); return MOD;}

"while"			{addToFIP(21, -1); return WHILE;}

"!="			{addToFIP(22, -1); return NE;}

"=="			{addToFIP(23, -1); return EQ;}

"<"			{addToFIP(24, -1); return LT;}

">"			{addToFIP(25, -1); return GT;}

"<="			{addToFIP(26, -1); return LE;}

">="			{addToFIP(27, -1); return GE;}

"if"			{addToFIP(28, -1); return IF;}

"switch"		{addToFIP(29, -1); return SWITCH;}

"case"			{addToFIP(30, -1); return CASE;}

":"			{addToFIP(31, -1); return COLON;}

"break"			{addToFIP(32, -1); return BREAK;}

"struct"		{addToFIP(33, -1); return STRUCT;}

"#include"		{addToFIP(34, -1); return INCLUDE;}

"<iostream>"		{addToFIP(35, -1); return IOSTREAM;}

"using"			{addToFIP(36, -1); return USING;}

"namespace"		{addToFIP(37, -1); return NAMESPACE;}

"std"			{addToFIP(38, -1); return STD;}

([+-]?{number}([.]{number})?)|(["][^"]*["])	{int index = add(&constsHashTable, yytext);  addToFIP(1, index); return CONST; }

{letter}({letter}|{digit}){0,249}		{int index = add(&idsHashTable, yytext);  addToFIP(0, index); return ID; }

\n						{ lineNum++; }

[ \t\r]+						;

.						{ hasErrors = 1; printf("[Line %d]: Invalid token %s\n", lineNum, yytext); return 0; }

%%