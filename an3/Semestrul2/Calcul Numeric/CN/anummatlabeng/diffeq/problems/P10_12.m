function [t,w,t2,w2]=P10_12
%P10_12 bataile inimii

tspan=[0,12];
opts=odeset('RelTol',1e-8);
[t,w]=ode45(@inima,tspan,[0.1,0.1]);
plot(w(:,1),w(:,2))
[t2,w2]=ode45(@inima,tspan,[0.87,2.1]);
figure(2)
plot(w2(:,1),w2(:,2))

function md=inima(t,y)
A=3;
epsilon=1;
md=[-1/epsilon*(y(1)^3-A*y(1)+y(2)); y(1)];
