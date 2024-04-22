%EXSPLINE takes 6 points on a sinusoid and plot 
%complete, second derivative, natural and deBoor splines
x = 0.1:2:10.1;
y = sin(x); 
yd = cos(x);
xx = 0.1:.025:10.1;
td=divdiff(x,y);
yy1=Newtonpol(td,x,xx);
[z,tdd]=divdiffdn(x,y,yd);
yy2=Newtonpol(tdd,z,xx);
hh=plot(x,y,'o',xx,sin(xx),'-',xx,yy1,'--',xx,yy2,'-.')
axis([-0.5,10.5,-1.5,1.5])
legend('nodes','sin','Lagrange','Hermite','Location','Southwest')