function [y,ni]=mycos(x,err)
%MYcos - calculul cosinusului
%apel [y,ni]=mycos(x,err)
%utilizeaza formula lui Taylor
%x - argumentul
%err - eroarea (implicit eps)
%y - valoarea aproximativa
%ni - numar de iteratii

if nargin<2, err=eps; end
if x==0, y=1; ni=0; end
x=reducereper(abs(x)); %reducere la perioada
%reducere la primul cadran
if x <=pi/2
    s2=1;
elseif x <=pi
    s2=-1; x=pi-x;
elseif x<=3*pi/2
    s2=-1; x=x-pi;
else
    s2=1; x=2*pi-x;
end
if x<=pi/4
    [y,ni]=cosred(x,err);
else
    [y,ni]=sinred(pi/2-x,err);
end
y=y*s2;