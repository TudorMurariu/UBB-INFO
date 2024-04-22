clf
t=linspace(0,2*pi,50);
x=6*cos(t);
y=sin(t);
%plot(x,y);
%hold on
xp=[-6,-5.8,-4.5,-3,0,3,5,6];
yp=[0,-1,-2.3,-2.28,-2.15,-1.9,-1.2,0];
t=0:length(xp)-1;
tp=linspace(0,length(xp)-1,100);
xg=pchip(t,xp,tp);
yg=pchip(t,yp,tp);
xl=[0,-3]; yl=[-1,sqrt(3)/2];
xc=[-1.5,-5]; yc=[0,0];
xdy1=xc; ydy1=[2,2];
xdy2=xc; ydy2=[2.5,2.5];
%plot(xg,yg)
a=20*pi/180;
A=[cos(a),-sin(a);sin(a),cos(a)];
P=A*[x;y];
G=A*[xg;yg];
L=A*[xl;yl];
C=A*[xc;yc];
Dy1=A*[xdy1;ydy1];
Dy2=A*[xdy2;ydy2];
plot(P(1,:),P(2,:),'k-',G(1,:),G(2,:),'k-',L(1,:),L(2,:),'k--',...
    C(1,:),C(2,:),'k-')
hold on
xcv=[C(1,1),C(1,1)]; ycv=[C(2,1),6];
plot(xcv,ycv,'k-','Linewidth',4)
plot([C(1,1),C(1,2)],[6,C(2,2)],'k-');
plot([C(1,1),L(1,1)],[6,L(2,1)],'k-')
axis equal