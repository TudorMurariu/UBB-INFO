function [x,v]=Poisson1DSOR(f,N)
%POISSON1DSOR - problema Poisson 1d
%-v''=f, x in [0,1]
%v(0)=v(1)=0
%f - functia
%N - nr. de puncte
%rezolvare cu SOR tridiagonal

h=1/(N+1); x=[1:N]'*h;
d=ones(N-1,1);
T=2*eye(N)-diag(d,1)-diag(d,-1);
b=h^2*f(x);
w=1; %w=relopt(T)
Nmax=10000;
era=1e-8; err=0;
v=SORtrid(T,b,w,era,err,Nmax);
x=[0;x;1];
v=[0;v;0];