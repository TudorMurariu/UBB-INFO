function [x,ni]=Jacobi(A,b,x0,err)
%JACOBI method
%call [x,ni]=Jacobi(A,b,x0,err)
%A - system matrix
%b - right hand side vector 
%x0 - starting vector 
%err - tolerance (default 1e-6)
%x - solution
%ni -number of actual iterations
  %parameter check
  if nargin < 4, err=1e-6; end
  if nargin < 3,  x0=zeros(size(b)); end
  [m,n]=size(A);
  if (m~=n) | (n~=length(b))
     error('ilegal size')
  end
  %compute T and c (prepare iterations)
  M=diag(diag(A));
  N=M-A;
  x=x0(:);
  ni=0;
  while norm(x-x0,inf)>err*norm(x,inf) || ni==0 
     x0=x;
     x=M\(N*x0+b); 
     ni++;
  end
endfunction 