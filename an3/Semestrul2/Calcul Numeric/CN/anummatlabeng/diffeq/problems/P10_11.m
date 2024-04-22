function P10_11
%P10_11  Dopul

L=3.75; %lungimea dopului (cm);
opt=odeset('Events',@expulzare,'AbsTol',1e-6);
sol=ode45(@dop,[0,1.5*L],[0,0],opt,L);
fprintf('Momentul expulzarii: t=%f s\n',sol.xe)
fprintf('viteza:%f m/s\n',sol.ye(1))

function dvdxdt=dop(t,vx,L)
g=9.81; %acceleratia gravitationala
q=20;   %raportul frecare -masa
d=5;    %lungimea gatului sticlei;
R=4;    %rata de crestere a presiunii (%)
gam=1.4;%constanta adiabatica a gazului din sticla

if vx(1)>=L
    dvdxdt(1)=0;
else
    dvdxdt(1)=g*(1+q)*(1+vx(2)/d)^(-gam)+R*t/100-1+q*vx(2)/(L*(1+q));
end
dvdxdt(2)=vx(1);
dvdxdt=dvdxdt';
function [vale,isterm,dir]=expulzare(t,vx,L)
vale=vx(2)-L;
isterm=0;
dir=1;
