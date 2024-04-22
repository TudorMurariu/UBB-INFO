%P5_7
n=5;
f=@(x) exp(x.^2);
fd=@(x) 2*x.*exp(x.^2);
x=linspace(0,1,n);
t=linspace(0,1,100);
y=f(x); yd=fd(x);
[a,b,c,d]=Hermitespline(x,y,yd);
yg=valspline(x,a,b,c,d,t);
k=1:n;
x2=cos((2*k-1)*pi/(2*n));
x2=sort((x2+1)/2);
y=f(x2); yd=fd(x2);
[a,b,c,d]=Hermitespline(x2,y,yd);
yg2=valspline(x2,a,b,c,d,t);
plot(t,f(t),t,yg,t,yg2)
legend('f','spline Hermite-noduri echidistante',...
    'spline Hermite- noduri Cebisev',0)