%elipsa

t=linspace(0,2*pi,70);
x=2.5*cos(t);
y=sin(t);
plot(x,y,'k-',[-4,4],[0,0],'k-',[0,0],[-1.5,1.5],'k-')
text(3,-0.5,'1/\beta','Fontsize',14)
text(-3,-0.5,'-1/\beta','Fontsize',14)
text(-1.5,1,'\itE','Fontsize',14)
text(0.5,0.6,'\rho(x)','Fontsize',14)
axis equal
