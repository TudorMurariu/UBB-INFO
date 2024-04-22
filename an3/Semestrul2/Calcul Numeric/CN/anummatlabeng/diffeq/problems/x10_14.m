function sol=x10_14
%P10_14 ghiulea sferica
v0=50;      %viteza initiala
tspan=[0,Inf];
opts=odeset('Events',@g,'RelTol',1e-6);

for tv=1:4
    for k=1:17
        teta0=k*pi/36;
        y0=[0,0,teta0,v0];
        sol(tv,k)=ode45(@ghiulea,tspan,y0,opts,tv);
    end
end

function dy=ghiulea(t,y,tv)
g=9.81;     %acceleratia gravitationala
ro=1.29;    %densitatea aerului
m=15;       %masa proiectilului(kg)
s=0.25;     %sectiunea proiectilului
c=0.2;      %coef. franare
switch tv   %calcul vant
    case 1, w=0;
    case 2, w=-10;
    case 3,
        if mod(floor(t),2)==0,
            w=10;
        else
            w=0;
        end;
    case 4, w=10*randn;
end
D=c*ro*s/2*((y(4)*cos(y(3))-w)^2+(y(4)*sin(y(3)))^2);
dy=[y(4)*cos(y(3));y(4)*sin(y(3));-g/y(4)*cos(y(3));...
    -D/m-g*sin(y(3))];

function [gstop, isterm, dir]=g(t,y,tv)
gstop=y(2);
isterm=1;
dir=-1;
