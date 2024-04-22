x =[-0.99, -0.76, -0.48, -0.18, 0.07, 0.2, ...
        0.46, 0.7, 0.84, 1.09, 1.45];
y = [0.39, 1.1,  0.61, -0.02, -0.33, 0.65, ...
        1.13, 1.46, 1.07, 1.2, 0.3];
plot(x,y,'o'); hold on
xi=linspace(min(x),max(x),100);
ys=interp1(x,y,xi,'spline');
yc=interp1(x,y,xi,'cubic');
h=plot(xi,ys,'-',xi,yc,'-.');
legend(h,'spline','cubic',4)
axis([-1.1,1.6,-0.8,1.6])
