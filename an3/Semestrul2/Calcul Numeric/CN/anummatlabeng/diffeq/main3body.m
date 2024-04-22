%
% Matlab code computing the restricted three-body problem
% (motion of a spacecraft in the Earth-Moon system)
%
%
clear;
tspan = [0 23.7];
%tspan = [0,294602];
%
M = 0.012277471; 

E = 1-M;
%
x0 = 1.15;
xdot0 = 0;
y0 = 0;
ydot0 = 0.0086882909;
%
vec0 = [x0 xdot0 y0 ydot0];
%
options = odeset('RelTol',1e-7,'AbsTol',[1e-7 1e-7 1e-7 1e-7]);
%
[t,y] = ode45(@r3body,tspan,vec0,options);
figure(1);
plot(y(:,1),y(:,3))
axis([-.8 1.2 -.8 .8]);grid on;
hold on
plot(-M,0,'o')
plot(E,0,'o');
hold off
xlabel('x');
ylabel('y')
text(0,0.05,'Earth')
text(0.9,-0.15,'Moon')
figure(2)
comet(y(:,1),y(:,3))