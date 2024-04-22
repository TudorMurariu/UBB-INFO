k=input('k=');
fp=inline('x+sin(pi*x.^2)');
x=linspace(-1,1,100);
y=fp(x);
yle=Legendreapprox(fp,x,k);
ycc=Chebyshevapprox(fp,x,k);
ycd=discrChebyshevapprox(fp,x,k);
plot(x,y,':', x,yle,'--', x,ycc,'-.',x,ycd,'-');
legend('f','Legendre', 'Continuous Cebyshev', 'Discrete Chebyshev',4)
title(['k=',int2str(k)],'Fontsize',14);
cl=Legendrecoeff(fp,k)
ccc=Chebyshevcoeff(fp,k)
ccd=discrChebyshevcoeff(fp,k)
