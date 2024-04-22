%exspli2
x = -4:4;
y = [0 .15 1.12 2.36 2.36 1.46 .49 .06 0];
cs = spline(x,[0 y 0]);
cs2 = spline(x,y);
xx = linspace(-4,4,101);
plot(x,y,'o',xx,ppval(cs,xx),'-',xx,ppval(cs2,xx),'--');