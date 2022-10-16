bits 32 ; asamblare si compilare pentru arhitectura de 32 biti

; definim punctul de intrare in programul principal
global start        

; declaram functiile externe necesare programului nostru
extern exit               ; indicam asamblorului ca ExitProcess exista, chiar daca noi nu o vom defini
import exit msvcrt.dll    ; exit este o functie care incheie procesul. Este definita in  msvcrt.dll
                          ; msvcrt.dll contine exit, printf si toate celelalte functii C-runtime importante

; segmentul de date in care se vor defini variabilele
segment data use32 class=data
    ; ...

; segmentul de cod
segment code use32 class=code
    start:
        ; ...
    
        ; exit(0)
        push    dword 0      ; push the parameter for exit onto the stack
        call    [exit]       ; call exit to terminate the program
