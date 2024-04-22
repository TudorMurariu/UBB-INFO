function [a,b,c,d]=Splineperiodic(x,f)
%SPLINEPERIODIC - determina coeficientii spline-ului cubic periodic
%apel [a,b,c,d]=Splineperiodic(x,f)
%x - abscisele
%f - ordonatele

n=length(x);

if any(diff(x)<0), [x,ind]=sort(x); else, ind=1:n; end
y=f(ind); x=x(:); y=y(:);
dx=diff(x);  ddiv=diff(y)./dx; %diferentele
dp=zeros(n,1); md=dp; di=dp; ds=dp;
dxs=dx(1:end-1); dxd=dx(2:end); 
ds=[0;dx(n-1);dxs];
di=[dxd;dx(1);0];
dp=2*[dx(n-1)+dx(1);dxs+dxd;dx(n-1)+dx(1)];
s1=dx(1)*ddiv(n-1)+dx(n-1)*ddiv(1);
md=3*[s1;dxd.*ddiv(1:end-1)+dxs.*ddiv(2:end);s1];
A=spdiags([di,dp,ds],-1:1,n,n);
A(1,n-1)=dx(1); A(n,2)=dx(end);
m=A\md;
d=y(1:end-1);
c=m(1:end-1);
a=[(m(2:end)+m(1:end-1)-2*ddiv)./(dx.^2)];
b=[(ddiv-m(1:end-1))./dx-dx.*a];
