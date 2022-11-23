@echo off
nasm -fobj %1
if %errorlevel% neq 0 exit /b %errorlevel%
alink -oPE -subsys console -entry start %~n1.obj
if %errorlevel% neq 0 exit /b %errorlevel%
%echo __________________________________________________________________________________
%~n1.exe