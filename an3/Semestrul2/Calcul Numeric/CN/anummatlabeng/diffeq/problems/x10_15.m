function [sol]=x10_15(yp)
%P10_15 parasutist
y0=1000;      %altitudine initiala
v0=50;
t0=0;
teta0=0;
tspan=[0,200];
opts=odeset('Events',@g,'RelTol',1e-6);
y0=[0,y0,v0,teta0];
sol=ode45(@parasutist,tspan,y0,opts,yp);
opts=odeset('Events',@g1,'RelTol',1e-6);
te=sol.xe
sol.ye(3)
tspan=[0,te,200];
sol=ode45(@parasutist,tspan,y0,opts,yp);
function dy=parasutist(t,y,yp)
g=9.81;     %acceleratia gravitationala
ro=1.2;     %densitatea aerului
M=80;       %masa parasutistului (kg)
s=0.5;      %sectiunea parasutistului
S=30;       %sectiunea parasutei
C=1;      %coef. franare
if y(2)>=yp
    A=s;
else
    A=S;
end
D=1/2*ro*C*A*y(3)^2;
dy=[y(3)*cos(y(4));y(3)*sin(y(4));-D/M-g*sin(y(4));...
    -g/y(3)*cos(y(4))];;

function [gstop, isterm, dir]=g(t,y,yp)
gstop=y(2)-yp;
isterm=1;
dir=-1;

function [gstop, isterm, dir]=g1(t,y,yp)
gstop=y(2);
isterm=1;
dir=-1;
