%Runge example for Hermite interpolation
close all
m=input('m=');
x=linspace(-1,1,1000);
f=@(x) 1./(1+25*x.^2);
fd=@(x) -50*x./(1+25*x.^2).^2;
xn=linspace(-1,1,m+1);
fv=f(xn); dfv=fd(xn);
[z,td]=divdiffdn(xn,fv,dfv);
vv=Newtonpol(td,z,x);
plot(x,f(x),x,vv,xn,f(xn),'o');
title('equispaced nodes')
figure(2)
k=1:m+1;
xn=cos((2*k-1)*pi/(2*m+2));
fv=f(xn); dfv=fd(xn);
[z,td]=divdiffdn(xn,fv,dfv);
vv=Newtonpol(td,z,x);
plot(x,f(x),x,vv,xn,f(xn),'o');
title('Chebyshev nodes')