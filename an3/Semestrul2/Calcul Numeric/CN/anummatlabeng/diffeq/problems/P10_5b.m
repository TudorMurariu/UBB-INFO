function [t,w,decel]=P10_5b
%P10_5 Star Trek

global M G c R 
tspan=[0,31*60];
M=6e24;     %masa planetei
G=6.67e-11; %constanta gravitatiei
c=0.004;    %ct franare
R=6.37e6;   %raza planetei
z0=127e3;   %altitudinea
y0=[sqrt(G*M/(R+z0)),0,z0,0];
opts=odeset('Events',@testdecel);
[t,w]=oderk(@startrek,tspan,y0,@RK547FM);
r=w(:,3)+R;
ro=1.3*exp(-w(:,3)/7600);
decel=-(G*M*sin(w(:,2))./r.^2-c*ro.*w(:,1).^2);
function dydt=startrek(t,y)
global M G c R 
r=y(3)+R;
ro=1.3*exp(-y(3)/7600);
dydt=[G*M*sin(y(2))/r^2-c*ro*y(1)^2;...
    (G*M/(r*y(1))-y(1))*cos(y(2))/r;...
    -y(1)*sin(y(2)); y(1)*cos(y(2))/r];
