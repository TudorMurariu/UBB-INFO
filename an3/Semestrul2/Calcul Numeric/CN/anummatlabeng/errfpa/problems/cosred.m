function [y,ni]=cosred(x,err)
%COSRED - cosinus cu argument redus
%apel [y,ni]=cosred(x,err)
%utilizeaza formula lui Taylor
%x - argumentul (ideal in (0,pi/4])
%err - eroarea (implicit eps)
%y - valoarea aproximativa
%ni - numar de iteratii

if nargin<2, err=eps; end
scos=1;
u=1;
for i=1:1000   
   u=-u*x^2/(2*i-1)/(2*i);
   scos=scos+u;
   if abs(u)<err*abs(scos)
      y=scos;   
      ni=i;
      return
   end
end
y=scos;
