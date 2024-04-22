function [x,la]=Power_method(A,x0,er,nmax)
%POWER_METHOD - for eigenvectors and eigen values
%call [x,la]=Power_method(A,x0)
%A - matrix
%x0 - starting vector
%er - relative error
%nmax - maximum no. of iterations
%la - dominant eigenvalue
%x - dominant eigenvector

if nargin<4, nmax=100; end
if nargin<3, er=eps; end
xv=x0/norm(x0); 
for i=1:nmax
    y=A*xv;
    xn=y/norm(y);
    la=xn'*A*xn;
    if norm(xn-xv)<er
        x=xn;
        return
    end
    xv=xn;
end
x=xn;
warning('iteration number exceeded')