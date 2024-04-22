%P5_6
clf
axis([0,1,0,1]); hold on
[x,y]=ginput;
n=length(x);
tn=linspace(0,1,n);
t=linspace(0,1,300);
%cu interpolare lagrange
xg=lagr(tn,x,t);
yg=lagr(tn,y,t);
%cu spline deBoor
[a1,b1,c1,d1]=Splinecubic(tn,x,3);
[a2,b2,c2,d2]=Splinecubic(tn,y,3);
xg2=valspline(tn,a1,b1,c1,d1,t);
yg2=valspline(tn,a2,b2,c2,d2,t);
plot(x,y,'o',xg,yg,'r-',xg2,yg2,'k-')
legend('noduri','Lagrange','spline deBoor',0)

