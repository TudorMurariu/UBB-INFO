function p10_3
%P10_3 atractorul Lorenz
clf
a=5; b=15; c=1;
tspan=[0,20];
opts=odeset('AbsTol',1e-4);
[t,w]=ode45(@Lorenz,tspan,[2,6,4],opts,a,b,c);
plot3(w(:,1),w(:,2),w(:,3),'g-')
opts=odeset('AbsTol',1e-5);
grid on
hold on
[t,w]=ode45(@Lorenz,tspan,[2.1,6,4],opts,a,b,c);
plot3(w(:,1),w(:,2),w(:,3),'r-')

function dydt=Lorenz(x,y,a,b,c)
dydt=[-a*y(1)+a*y(2);b*y(1)-y(2)-y(1)*y(3);...
    -c*y(3)+y(1)*y(2)];
