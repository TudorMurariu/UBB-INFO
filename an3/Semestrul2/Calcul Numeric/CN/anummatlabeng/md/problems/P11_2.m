%P11_2
u=linspace(0,1,15); v=u;
f=@(x,y) x.*exp(x.^2+y.^2);
f10=@(x,y) exp(x.^2+y.^2).*(1+2*x.^2);
f01=@(x,y) 2*x.*y.*exp(x.^2+y.^2);
f11=@(x,y) 2*y.*exp(x.^2+y.^2).*(1+2*x.^2);
[X,Y,Z]=HermiteBoolSum(u,v,f,f10,f01,f11);
subplot(1,2,1)
surf(X,Y,Z)
xlabel('x')
ylabel('y')
Z2=f(X,Y);
subplot(1,2,2)
surf(X,Y,Z2);
xlabel('x')
ylabel('y')
