% Matlab code computing the restricted three-body problem
% (motion of a spacecraft in the Earth-Moon system)
%
%
clear;
tspan = [0,6.192169331319640];
M = 1/82.45; E = 1-M;
%
x0 = 1.2; xdot0 = 0; y0 = 0;
ydot0 = -1.049357509830320;
%
vec0 = [x0 xdot0 y0 ydot0];
%
options = odeset('RelTol',1e-6,'AbsTol',[1e-6 1e-6 1e-6 1e-6]);
%
[t,y] = ode45(@r3body,tspan,vec0,options);
plot(y(:,1),y(:,3))
axis([-1.5 1.5 -.8 .8]);grid on;
hold on
plot(-M,0,'o')
plot(E,0,'o');
hold off
xlabel('x');
ylabel('y')
text(0,0.1,'Earth','FontSize',16)
text(0.9,-0.1,'Moon','FontSize',16)