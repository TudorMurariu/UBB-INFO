function P10_8
%P10_8 Saritura lui Bob Beamon, JO mexico, 1968

v0=10;      %viteza initiala (m/s)
roMexic=0.94;    %densitatea aerului Mexic (kg/m^3)
tspan=[0,Inf];
opts=odeset('Events',@g);

xf1=saritura(v0,roMexic);
ronm=1.29;    %densitatea aerului niv. marii
xf2=saritura(v0,ronm);
v01=fzero(@(v) saritura(v,roMexic)-8.90,v0);
xf3=saritura(v01,roMexic);
xf4=saritura(v01,ronm);
rez=[v0,22.5,roMexic,xf1;...
    v0,22.5,ronm,xf2;...
    v01,22.5,roMexic, xf3;...
    v01,22.5,ronm, xf4] 

function d=saritura(v0,ro)
tspan=[0,Inf];
opts=odeset('Events',@g);
teta0=pi/8; %unghiul initial (radiani)
[t,w,tf,xf]=ode45(@BobBeamon,tspan,[0,0,teta0,v0],opts,v0,ro);
d=xf(1);

function yd=BobBeamon(t,y,v0,ro)
g=9.81; %acceleratia gravitationala (m/s^2)
m=80;   %masa saritorului (kg)
c=0.72;  %coeficientul de franare
s=0.50; %aria sectiunii saritorului (m^2)
D=c*ro*s/2*y(4)^2;
yd=[y(4)*cos(y(3));y(4)*sin(y(3));...
    -g/y(4)*cos(y(3)); -D/m-g*sin(y(3))];
    
function [gstop, isterm, dir]=g(t,y,vo,ro)
gstop=y(2);
isterm=1;
dir=-1;
