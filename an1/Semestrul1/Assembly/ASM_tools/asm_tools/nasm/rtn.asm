;NASM Win32 resource sample
; compile and link with 
;
;nasm -fobj rtn.asm
;
;alink -oPE rtn win32.lib rtn.res


[extern DialogBoxParamA]
[extern GetModuleHandleA]
[extern ExitProcess]
[extern EndDialog]

WM_INITDIALOG equ 0110h
WM_COMMAND equ 0111h

IDOK equ 1

[segment code public use32 class='CODE']

..start:
enter 0,0
push byte 0
call GetModuleHandleA
mov [handle],eax
push byte 0
push dword DProc
push byte 0
push dword string
push byte 0
call DialogBoxParamA
push dword [handle]
call ExitProcess
leave
ret

DProc:
%define lparam ebp+20
%define wparam ebp+16
%define msg ebp+12
%define hdlg ebp+8
 enter 0,0
 mov eax,[msg]
 cmp eax,WM_INITDIALOG
 je @@wm_init
 cmp eax,WM_COMMAND
 je @@wm_command
@@unhandled:
 xor eax,eax
 leave
 ret 16
@@wm_init:
 mov eax,1
 leave
 ret 16
@@wm_command:
 cmp dword [wparam],IDOK
 jne @@unhandled
 push byte 1
 push dword [hdlg]
 call EndDialog
 mov eax,1
 leave
 ret 16

[segment data public]
handle dd 0
string db 'ABOUTDLG',0
