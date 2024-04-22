function [a,b,c,d]=Hermitespline(x,f,fd);
%HERMITESPLINE - Coeficienti spline Hermite

n=length(x);
if any(diff(x)<0), [x,ind]=sort(x); else, ind=1:n; end
y=f(ind); m=fd(ind); x=x(:); y=y(:); m=m(:);
dx=diff(x);  ddiv=diff(y)./dx;
d=y(1:end-1);
c=m(1:end-1);
a=[(m(2:end)+m(1:end-1)-2*ddiv)./(dx.^2)];
b=[(ddiv-m(1:end-1))./dx-dx.*a];