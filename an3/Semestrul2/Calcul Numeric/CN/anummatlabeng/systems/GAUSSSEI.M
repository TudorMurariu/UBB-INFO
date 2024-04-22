function  [z,ni]=gausssei(A,b,ea,er,nitmax)
%metoda lui Gauss si Seidel
%apel  [z,ni]=Gausssei(A,b,ea,er,nitmax)
% z - solutia
% ni -numarul de iteratii
% A - matricea sistemului
% b - termenii liberi
% ea - eroarea absoluta (implicit 1e-3)
% er -eroarea relativa (implicit 0)
% nitmax -numarul maxim de iteratii (implicit 20)
if nargin<5 
   nitmax=20;
end
if nargin<4
   er=0;
end;
if nargin < 3
   ea=1e-3;
end
n=length(b);
x=zeros(n,nitmax+1);
M=tril(A);
N=M-A;
T=inv(M)*N;
c=inv(M)*b;
for  k=2:nitmax+1
  x(:,k)=T*x(:,k-1)+c;
  if norm(x(:,k)-x(:,k-1),inf) < ea + er*norm(x(:,k),inf)
    z=x(:,k);
    ni=k;
    return;
  end
end
error('prea multe iteratii')
