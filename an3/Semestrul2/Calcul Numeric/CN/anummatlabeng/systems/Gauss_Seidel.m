function [z,ni]=Gauss_Seidel(A,b,err,nitmax)
%metoda Gauss_Seidel
%apel [z,ni]=Gauss_Seidel(A,b,err,nitmax)
%parametri
%A - matricea sistemului
%b - vectorul termenilor liberi
%err - toleranta (implicit 1e-3)
%nitmax - numarul maxim de iteratii (implicit 50)
%z - solutia
%ni -numarul de iteratii realizat efectiv
if nargin < 4 %verificare parametri
   nitmax=50;
end
if nargin < 3 
   err=1e-3;
end
[m,n]=size(A);
if (m~=n) | (n~=length(b))
   error('dimensiuni ilegale')
end
%calculul lui T si c (pregatirea iteratiilor)
x=zeros(n,nitmax+1);
M=tril(A);
N=M-A;
T=M\N;
c=M\b;
alfa=norm(T,inf);
for i=1:nitmax
   x(:,i+1)=T*x(:,i)+c;
   if norm(x(:,i+1)-x(:,i),inf)<(1-alfa)/alfa*err
      z=x(:,i+1);
      ni=i;
      return
   end
end
error('prea multe iteratii')