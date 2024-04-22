%P5_16
clf
f=@(x) x+sin(pi*x.^2);
n=input('n=');
x=linspace(-1,1,150);
y1=approxChebyshevdiscr2(f,x,n);
y2=approxChebyshevdiscr(f,x,n);
plot(x,f(x),x,y1,x,y2)
s=sprintf('Aproxim\\u{a}ri Ceb\\^{\\i}\\c{s}ev discrete, $n=%d$',n);
legend('f','in puncte de extrem','in radacini',...
    'Location','Best')
title(s,'Interpreter','LaTeX','FontSize',14)