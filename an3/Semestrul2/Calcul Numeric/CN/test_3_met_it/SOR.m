function [x,ni]=SOR(A,b,omega,x0,err)
%Successive overrelaxation (SOR) method
%call [z,ni]=relax(A,b,omega,x0,err,nitmax)
%A - system matrix
%b - right hand side vector
%omega - relaxation parameter
%x0 - starting vector
%err - tolerance (default 1e-6)
%x - solution
%ni -actual number of iterations
  %parameter check
  if nargin < 5, err=1e-6; end
  if nargin < 4, x0=zeros(size(b)); end
  if (omega<=0) || (omega>=2)
      error('ilegal relaxation parameter')
  end
  [m,n]=size(A);
  if (m~=n) || (n~=length(b))
      error('ilegal size')
  end
  %compute T and c (prepare iterations)
  M=1/omega*diag(diag(A))+tril(A,-1);
  N=M-A;
  x=x0;
  ni=0;
  while norm(x-x0,inf)>err*norm(x,inf) || ni==0 
      x0=x;
      x=M\(N*x0+b); 
      ni++;
  end
endfunction