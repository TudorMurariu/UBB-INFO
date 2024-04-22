function [x,iter] = mysqrt(a,tol)
%MYSQRT   Square root by Newton's method 
%         X = SQRTN(A,TOL) computes the square root of
%         A by Newton's (Heron's) method
%         assume A  >= 0.
%         TOL is the tolerance (default EPS).
%         [X,ITER] = SQRTN(A,TOL) returns also the number 
%         of iterations ITER required 

if nargin < 2, tol = eps; end

x = a;

for k=1:50 
    xold = x;
    x = (x + a/x)/2;
    if abs(x-xold) <= tol*abs(x)
        iter=k; return; 
    end
end
error('Not converged after 50 iterations')
