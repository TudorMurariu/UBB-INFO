function [z,ni]=Gauss_seidel(A,b,x0,err,nitmax)
%metoda Gauss_Seidel
%apel [z,ni]=Gauss_Seidel(A,b,x0,err,nitmax)
%parametri
%A - matricea sistemului
%b - vectorul termenilor liberi
%err - toleranta (implicit 1e-3)
%nitmax - numarul maxim de iteratii (implicit 50)
%z - solutia
%ni -numarul de iteratii realizat efectiv
if nargin < 5 %verificare parametri
   nitmax=50;
end
if nargin < 4 
   err=1e-3;
end
if nargin < 3
   x0=zeros(size(b));
end

[m,n]=size(A);
if (m~=n) || (n~=length(b))
   error('dimensiuni ilegale')
end
if nargin<3, x0=zeros(n,1); end
%calculul lui T si c (pregatirea iteratiilor)
x=zeros(n,1);
M=tril(A);
N=M-A;
for i=1:nitmax
   xn=M\(N*x+b);
   if norm(xn-x,inf)< err*norm(xn,inf) %(1-alfa)/alfa*err
      z=xn;
      ni=i;
      return
   end
   x=xn;
end
error('prea multe iteratii')