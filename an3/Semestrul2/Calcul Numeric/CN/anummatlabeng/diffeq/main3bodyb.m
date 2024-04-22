%
% Matlab code computing the restricted three-body problem
% (motion of a spacecraft in the Earth-Moon system)
%
%
clear; close all;
%tspan = [0 23.7];
tspan = [0 29.4602]; %experiment
%
M = 0.012277471; E = 1-M;
%
x0 = 1.15;
xdot0 = 0;
y0 = 0;
ydot0 = 0.0086882909;
vec0 = [x0 xdot0 y0 ydot0];
scal=1e-1; %1e0, 1e1, 1e2, 1e3,....
options = odeset('RelTol',1e-6*scal,'AbsTol',[1e-6 1e-6 1e-6 1e-6]*scal);
[t,y] = ode45(@r3body,tspan,vec0,options);
figure(1);plot(y(:,1),y(:,3))
axis([-.8 1.2 -.8 .8]);grid on;
hold on
plot(-M,0,'o')
plot(E,0,'o');
hold off
xlabel('x'): ylabel('y')
text(0,0.1,'Earth','FontSize',16)
text(0.9,-0.15,'Moon','FontSize',16)
figure(2);
shg
axis([-.8 1.2 -.8 .8]);grid on;
hold on
plot(-M,0,'o')
plot(E,0,'o');

xlabel('x'): ylabel('y')
text(0,0.1,'Earth','FontSize',16)
text(0.9,-0.15,'Moon','FontSize',16)
comet(y(:,1),y(:,3))
hold off