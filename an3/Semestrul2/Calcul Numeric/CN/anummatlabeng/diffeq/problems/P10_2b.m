%problema 10_2, (b)

eca=@(x,y) [y(2);0.032-0.4*y(2)^2];
se=@(x) [5/2*log(cosh(2*sqrt(2)*x/25))+30,...
    sqrt(2)/5*tanh(2*sqrt(2)*x/25)];
[t,w]=ode45(eca,[0,20],[30,0]);
sev=se(t);
plot(t,w,t,sev);