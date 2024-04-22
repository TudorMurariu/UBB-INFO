function [x,ni]=Jacobi(A,b,x0,err,nitmax)
%JACOBI method
%call [x,ni]=Jacobi(A,b,x0,err,nitmax)
%parameters
%A - system matrix
%b - right hand side vector 
%x0 - starting vector 
%err - tolerance (default 1e-3)
%nitmax - maximum number of iterations (default 50)
%x - solution
%ni -number of actual iterations

%parameter check
if nargin < 5, nitmax=50; end
if nargin < 4, err=1e-3; end
if nargin <3,  x0=zeros(size(b)); end
[m,n]=size(A);
if (m~=n) | (n~=length(b))
   error('ilegal size')
end
%compute T and c (prepare iterations)
M=diag(diag(A));
N=M-A;
x=x0(:);
for i=1:nitmax
   x0=x;
   x=M\(N*x0+b);  
   if norm(x-x0,inf)<err*norm(x,inf)  
      ni=i;
      return
   end
end
error('iteration number exceeded')