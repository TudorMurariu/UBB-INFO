function p10_6(err)
%P10_6 Cometa Halley

if nargin<1, err=1e-7; end
tspan=[0,100];
%pozitia initiala
y0=[0.325514;-0.459460;0.166229;-9.096111;-6.916686;-1.305721];
opts=odeset('Events',@testperiheliu,'RelTol',err);
[t,w,te,ye,ie]=ode45(@cometa_halley,tspan,y0,opts,y0);
plot3(w(:,1),w(:,2),w(:,3),0,0,0,'.',y0(1),y0(2),y0(3),'.r')
axis image
title('Cometa Halley','Fontsize',16)
grid on
fprintf('urmatorul periheliu:%10.5f\n',te(end)+1986)
function dydt=cometa_halley(t,y,y0)
mu=4*pi^2;
r=sqrt(y(1)^2+y(2)^2+y(3)^2);
dydt=[y(4);y(5);y(6);-mu*y(1)/r^3; -mu*y(2)/r^3; -mu*y(3)/r^3];

function [val,isterm,dir] = testperiheliu(t,y,y0)
d = y(1:3)-y0(1:3);
v=y(4:6);
val=d'*v;
isterm = (t~=0);
dir = 1;
