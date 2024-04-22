a = 12; b=5;
t=0:0.05:10*pi;
x = (a+b)*cos(t)-b*cos((a/b+1)*t);
y =(a+b)*sin(t)-b*sin((a/b+1)*t);
plot(x,y)
axis equal
axis([-25 25 -25 25])
grid on 
title('Epicicloid: $a=12$, $b=5$',...
    'Interpreter','LaTeX','FontSize',16)
xlabel('$x(t)$','Interpreter','LaTeX','FontSize',14),
ylabel('$y(t)$','Interpreter','LaTeX','FontSize',14)
