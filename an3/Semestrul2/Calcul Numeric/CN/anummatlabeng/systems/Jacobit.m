function [z,x,ni]=Jacobit(A,b,x0,err,nitmax)
%metoda lui Jacobi
%apel [z,x,ni]=Jacobit(A,b,x0,err,nitmax)
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
if (m~=n) | (n~=length(b))
   error('dimensiuni ilegale')
end
%calculul lui T si c (pregatirea iteratiilor)
x=zeros(n,nitmax+1);
M=diag(diag(A));
N=M-A;
T=inv(M)*N;
c=inv(M)*b;
alfa=norm(T,inf);
x(:,1)=x0(:);
for i=1:nitmax
   x(:,i+1)=T*x(:,i)+c;
   if norm(x(:,i+1)-x(:,i),inf)<(1-alfa)/alfa*err*norm(x(:,i+1),inf)
      z=x(:,i+1);
      ni=i;
      return
   end
end
error('prea multe iteratii')