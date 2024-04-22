function [x,v]=Poisson1D(f,N)
%POISSON1D - problema Poisson 1d
%-v''=f, x in [0,1]
%v(0)=v(1)=0
%f - functia
%N - nr. de puncte
%rezolvare cu Cholesky tridiagonal

h=1/(N+1); x=[1:N]'*h;
d=ones(N-1,1);
T=2*eye(N)-diag(d,1)-diag(d,-1);
b=h^2*f(x);
R=Choleskytrid(T);
v=R\(R'\b);
x=[0;x;1];
v=[0;v;0];