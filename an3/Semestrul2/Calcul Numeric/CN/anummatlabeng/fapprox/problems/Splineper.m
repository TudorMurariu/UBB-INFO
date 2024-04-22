function [a,b,c,d]=Splineper(x,f)
%SPLINECUBIC - determina coeficientii spline-ului cubic
%apel [a,b,c,d]=Splineperiodic(x,f)
%x - abscisele
%f - ordonatele

n=length(x);

if any(diff(x)<0), [x,ind]=sort(x); else, ind=1:n; end
y=f(ind); x=x(:); y=y(:);
dx=diff(x);  ddiv=diff(y)./dx; %diferentele
dp=zeros(n,1); di=dp; ds=dp; md=dp;
dp(1)=2*(dx(n-1)+dx(1));
ds(2)=dx(n-1);
md(1)=3*(dx(1)*ddiv(n-1)+dx(n-1)*ddiv(1));
for k=2:n-1
    dp(k)=2*(dx(k-1)+dx(k));
    ds(k+1)=dx(k-1);
    di(k-1)=dx(k);
    md(k)=3*(dx(k)*ddiv(k-1)+dx(k-1)*ddiv(k));
end
dp(n)=dp(1);
di(n-1)=dx(1);
md(n)=md(1);
A=spdiags([di,dp,ds],-1:1,n,n);
A(1,n-1)=dx(1); A(n,2)=dx(end);
m=A\md;
d=y(1:end-1);
c=m(1:end-1);
a=[(m(2:end)+m(1:end-1)-2*ddiv)./(dx.^2)];
b=[(ddiv-m(1:end-1))./dx-dx.*a];

