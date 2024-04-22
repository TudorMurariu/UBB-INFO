function [X,Y,Z]=Coons(g1,g2,g3,g4,a,b,N,c,d,M)
%COONS - suprafata Coons
%S:[a,b]x[c,d]->R
%S(a,y)=g1(y), S(b,y)=g2(y)
%S(x,c)=g3(x), S(x,d)=g4(x);
%apel [X,Y,Z]=Coons(g1,g2,g3,g4,a,b,N,c,d,M)
%g1:4 - curbele
%a,b,c,d - intervalele
%N,M - rezolutia

if nargin<10, M=15; end
if nargin<9, d=1; end
if nargin<8, c=0; end
if nargin<7, N=15; end
if nargin<6, b=1; end
if nargin<5, a=0; end
    
x=linspace(a,b,N); y=linspace(c,d,N);
[X,Y]=meshgrid(x,y);
Z=(1-X).*g1(Y)+X.*g2(Y)+(1-Y).*g3(X)+Y.*g4(X)-...
    (1-X).*(1-Y)*g1(0)-(1-X).*Y*g1(1)-X.*(1-Y)*g2(0)-X.*Y*g2(1);