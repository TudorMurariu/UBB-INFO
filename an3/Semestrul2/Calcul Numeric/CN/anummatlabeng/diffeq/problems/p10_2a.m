%problema 10_2, (a)

eca=@(x,y) 1/4*y*(1-1/20*y);
se=@(x) 20/(1+19*exp(-x/4));
[t,w]=ode45(eca,[0,20],1);
plot(t,w,t,se(t));