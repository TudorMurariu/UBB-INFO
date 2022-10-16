; asamblare 			nasm -fobj exemplu.asm
; linkeditare 			alink -oPE -subsys console -entry start exemplu.obj
; depanare  			ollydbg exemplu.exe
; programe necesare 	http://www.nasm.us + alink: alink.sourceforge.net/download.html + http://www.ollydbg.de

bits 32

global start
extern ExitProcess, printf
import ExitProcess kernel32.dll
import printf msvcrt.dll

segment code use32 class=CODE


start:
    push dword msg
    call [printf]
    add esp, 4

    push dword 0
    call [ExitProcess]


segment data use32 class=DATA
    msg: db "Mesaj!", 13, 10, 0
    