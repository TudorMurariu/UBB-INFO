function [t,w,te,ye,ie]=P10_5
%P10_5 Star Trek

global M G c R 
tspan=[0,Inf];
M=6e24;     %masa planetei
G=6.67e-11; %constanta gravitatiei
c=0.004;    %ct franare
R=6.37e6;   %raza planetei
z0=127e3;   %altitudinea
y0=[sqrt(G*M/(R+z0)),0,z0,0];
opts=odeset('Events',@testdecel);
[t,w,te,ye,ie]=ode45(@startrek,tspan,y0,opts);

function dydt=startrek(t,y)
global M G c R 
r=y(3)+R;
ro=1.3*exp(-y(3)/7600);
dydt=[G*M*sin(y(2))/r^2-c*ro*y(1)^2;...
    (G*M/(r*y(1))-y(1))*cos(y(2))/r;...
    -y(1)*sin(y(2)); y(1)*cos(y(2))/r];
function [val,isterm,dir]=testdecel(t,y)
global M G c R 
r=y(3)+R;
ro=1.3*exp(-y(3)/7600);
vv=-(G*M*sin(y(2))/r^2-c*ro*y(1)^2);
if vv<=5*9.81
    val=1;
else
    val=0;
end
isterm=1;
dir=1;