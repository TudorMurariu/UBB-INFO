function [xn,ni]=Gauss_seidel(A,b,x0,err)
%metoda Gauss_Seidel
%apel [z,ni]=Gauss_Seidel(A,b,x0,err)
%A - matricea sistemului
%b - vectorul termenilor liberi
%err - toleranta (implicit 1e-6)
%z - solutia
%ni -numarul de iteratii realizat efectiv
  %parameter check
  if nargin < 4 
     err=1e-6;
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
  ni = 0;
  xn=x(:);
  while ni==0 || norm(xn-x,inf) > err*norm(xn,inf)
   x=xn;
   xn=M\(N*x+b);
   ni++;
  end
endfunction