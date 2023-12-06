%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexer.h"

extern int yylex();
extern FILE* yyin;

void yyerror(const char* s);

%}

%token INCLUDE IOSTREAM USING NAMESPACE STD
%token INT FLOAT STRING STRUCT
%token LPAREN RPAREN LBRACE RBRACE COMMA SEMICOLON ASSIGN
%token RIGHT_SHIFT LEFT_SHIFT PLUS MINUS MUL DIV MOD
%token NE EQ LT GT LE GE
%token CIN COUT WHILE IF SWITCH CASE COLON BREAK
%token CONST ID
%token UNKNOWN

%%

program: antet_program functie

antet_program: INCLUDE IOSTREAM USING NAMESPACE STD SEMICOLON

functie: antet_functie corp

antet_functie: tip ID LPAREN lista_decl RPAREN
            | tip ID LPAREN RPAREN

tip: INT | FLOAT | STRING | STRUCT

struct: STRUCT ID LBRACE declarare_structura RBRACE SEMICOLON

declarare_structura: declarare declarare_structura
                  | declarare

lista_decl: declarare_fct COMMA lista_decl
          | declarare_fct

declarare_fct: tip ID

corp: LBRACE instr_compusa RBRACE

instr_compusa: instr instr_compusa
            | instr

instr: declarare
    | atribuire
    | instr_citire
    | instr_afisare
    | instr_while
    | instr_if
    | instr_switch

declarare: tip ID SEMICOLON
         | tip atribuire

atribuire: ID ASSIGN expr_aritmetica SEMICOLON

instr_citire: CIN RIGHT_SHIFT ID SEMICOLON

instr_afisare: COUT LEFT_SHIFT expr_aritmetica SEMICOLON

expr_aritmetica: expr_aritmetica op_artimetic expr_aritmetica
              | ID
              | CONST

op_artimetic: PLUS | MINUS | MUL | DIV | MOD

instr_while: WHILE LPAREN conditie RPAREN corp

conditie: expr_aritmetica op_relational expr_aritmetica
        | expr_aritmetica

op_relational: NE | EQ | LT | GT | LE | GE

instr_if: IF LPAREN conditie RPAREN corp

instr_switch: SWITCH LPAREN ID RPAREN LBRACE lista_instr_case RBRACE

lista_instr_case: instr_case lista_instr_case
                | instr_case

instr_case: CASE CONST COLON instr_compusa BREAK SEMICOLON

%%

void yyerror(const char* s) {
    fprintf(stderr, "Syntax error on line %d\n", lineNum);
    exit(EXIT_FAILURE);
}

int main(int argc, char** argv) {
    FILE *fp;
    fp = fopen(argv[1], "r");
    initHashTable(&idsHashTable, 13);
    initHashTable(&constsHashTable, 13);

    yyin = fp;
    yyparse();

    if (hasErrors == 0) {
        printFIP();
        printTS(&idsHashTable, &constsHashTable);
        printf("Program corect sintactic\n");
    }

    freeHashTable(&idsHashTable);
    freeHashTable(&constsHashTable);

    printf("Lines num: %d\n", lineNum);

    fclose(fp);
    return 0;
}
