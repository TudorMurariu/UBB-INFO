%EXSPLINE ia 6 puncte de pe o sinusoida si
%construieste spline-ul deBoor si complet
x = 0:2:10;
y = sin(x); yc=[cos(0),y,cos(10)];
xx = 0:.01:10;
yy = spline(x,y,xx);
yc = spline(x,yc,xx);
plot(x,y,'o',xx,sin(xx),'-',xx,yy,'--',xx,yc,'-.')
axis([-0.5,10.5,-1.3,1.3])
legend('noduri','sin','deBoor','complet',4)