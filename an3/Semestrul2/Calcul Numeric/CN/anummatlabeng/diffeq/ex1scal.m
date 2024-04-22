%ex1scal
tspan = [0,3]; yzero=0;
[t,y]=ode45(@f1scal,tspan,yzero);
plot(t,y,'k--*')
xlabel('t'), ylabel('y(t)')