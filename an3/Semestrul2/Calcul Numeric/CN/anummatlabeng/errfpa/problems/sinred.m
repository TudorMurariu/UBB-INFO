function [y,ni]=sinred(x,err)
%SINRED - sinus cu argument redus
%apel [y,ni]=sinred(x,err)
%utilizeaza formula lui Taylor
%x - argumentul (ideal in [0,pi/4])
%err - eroarea (implicit eps)
%y - valoarea aproximativa
%ni - numar de iteratii

if nargin<2, err=eps; end
ssin=x;
u=x;
for i=1:1000    
   u=-u*x^2/(2*i)/(2*i+1);
   ssin=ssin+u;
   if abs(u)<err*abs(ssin)
      y=ssin;   
      ni=i;
      return
   end
end
y=ssin;
error('prea multe iteratii')