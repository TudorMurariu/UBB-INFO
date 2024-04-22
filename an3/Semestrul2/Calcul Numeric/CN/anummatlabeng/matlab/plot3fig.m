t = 0:pi/50:6*pi;
expt = exp(-0.1*t);
xt = expt.*cos(t); yt = expt.*sin(t);
plot3(xt, yt, t), grid on
xlabel('x(t)'), ylabel('y(t)'), zlabel('z(t)')
title('plot3 {\itexample}','FontSize',14)