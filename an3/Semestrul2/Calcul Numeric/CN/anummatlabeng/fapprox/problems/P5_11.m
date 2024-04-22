%P5_5
clf
x=-5:5;
y=[0,0,0,1,1,1,0,0,0,0,0];
t=linspace(-5,5,200);
c=polyfit(x,y,7);
y1=polyval(c,t);
y2=lagr(x,y,t);
stairs(x,y,'-o'); hold on
plot(t,y1,'r-',t,y2,'k-');
legend('nodurile','mcmmp','Lagrange',0)