function [y,ni]=mysin(x,err)
%MYSIN - calculul sinusului
%apel [y,ni]=mysin(x,err)
%utilizeaza formula lui Taylor
%x - argumentul
%err - eroarea (implicit eps)
%y - valoarea aproximativa
%ni - numar de iteratii

if nargin<2, err=eps; end
if x==0, y=0; ni=0; return; end
if x<0, s1=-1; else s1=1; end
x=reducereper(abs(x)); %reducere la perioada
%reducere la primul cadran
if x==0, y=0; ni=0; return; end
if x <=pi/2
    s2=s1;
elseif x <=pi
    s2=s1; x=pi-x;
elseif x<=3*pi/2
    s2=-s1; x=x-pi;
else
    s2=-s1; x=2*pi-x;
end
if x<=pi/4
    [y,ni]=sinred(x,err);
else
    [y,ni]=cosred(pi/2-x,err);
end
y=y*s2;