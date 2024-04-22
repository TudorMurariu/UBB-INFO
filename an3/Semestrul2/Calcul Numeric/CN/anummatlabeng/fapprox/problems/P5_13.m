%P5_13
f=@(x) (1+x)./(1+2*x+3*x.^2);
fd=@(x) 1./(1+2*x+3*x.^2)-(1+x)./(1+2*x+3*x.^2).^2.*(2+6*x);
x=0:5; y=f(x); yd=fd(x);
t=linspace(0,5,100);
y1=lagr(x,y,t);
y3=spline(x,y,t);
[z,td]=difdivnd(x,y,yd);
y2=pNewton(td,z,t);
yt=f(t);
plot(x,y,'o',t,yt,t,y1,t,y2,t,y3);
legend('nodurile','f','Lagrange','Hermite','spline',0)
title('Interpolan\c{t}i pentru $f(x)=\frac{1+x}{1+2x+3x^2}$',...
    'Interpreter','LaTeX','Fontsize',14)
figure(2)
plot(t,yt-y1,t,yt-y2,t,yt-y3)
title('erorile','Fontsize',14)
legend('Lagrange','Hermite','spline',0)