function [x,iter] = sqrtn(a,tol)
%SQRTN    Radical cu metoda lui Newton.
%         X = SQRTN(A,TOL) calculeaza radacina patrata a lui
%         A prin metoda lui Newton (numita si metoda lui Heron).
%         presupunem ca A  >= 0.
%         TOL este toleranta (implicit EPS).
%         [X,ITER] = SQRTN(A,TOL) returneaza numarul de 
%         iteratii ITER necesare.

if nargin < 2, tol = eps; end

x = a;
iter = 0;
xdiff = inf;
fprintf(' k             x_k            er. relativa\n')

for k=1:50 
    iter = iter + 1;
    xold = x;
    x = (x + a/x)/2;
    xdiff = abs(x-xold)/abs(x);
    fprintf('%2.0f:  %20.16e  %9.2e\n', iter, x, xdiff)
    if xdiff <= tol, return, end
end
error('Nu s-a atins precizia dupa 50 de iteratii.')
