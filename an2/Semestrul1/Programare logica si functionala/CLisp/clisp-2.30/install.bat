@echo off

echo this will install CLISP on your system,
echo binding file types FAS, MEM and LISP with CLISP.
echo it will also create a CLISP.BAT file on your desktop.
echo press C-c to abort
pause

if exist src\install.lisp goto installsrc
if exist install.lisp goto install
goto notfound
:installsrc
lisp.exe -B . -M lispinit.mem -norc -C src\install.lisp
goto exit
:install
lisp.exe -B . -M lispinit.mem -norc -C install.lisp
goto exit
:notfound
echo Sorry, install.lisp not found, cannot install
:exit
pause
