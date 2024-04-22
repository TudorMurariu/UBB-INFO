function [x,ni]=relax(A,b,omega,x0,err,nitmax)
%RELAX Successive overrelaxation (SOR) method
%call [z,ni]=relax(A,b,omega,err,nitmax)
%parameters
%A - system matrix
%b - right hand side vector
%omega - relaxation parameter
%x0 - starting vector
%err - tolerance (default 1e-3)
%nitmax - maximum number of iterations (default 50)
%z - solution
%ni -actual number of iterations

%check parameters
if nargin < 6, nitmax=50; end
if nargin < 5, err=1e-3; end
if nargin < 4, x0=zeros(size(b)); end
if (omega<=0) | (omega>=2)
    error('ilegal relaxation parameter')
end
[m,n]=size(A);
if (m~=n) | (n~=length(b))
    error('ilegal size')
end
%compute T and c (prepare iterations)
M=1/omega*diag(diag(A))+tril(A,-1);
N=M-A;
T=M\N;
c=M\b;
alfa=norm(T,inf)
x=x0(:);
for i=1:nitmax
    x0=x;
    x=T*x0+c;
    if norm(x-x0,inf)<(1-alfa)/alfa*err
        ni=i;
        return
    end
end
error('iteration number exceeded')