%P5_16
%varianta ce calculeaza si coeficientii
clf
f=@(x) x+sin(pi*x.^2);
n=input('n=');
x=linspace(-1,1,150);
c1=coeffChebyshevdiscr2(f,n);
c2=coeffChebyshevdiscr(f,n);
y1=evalChebyshev(c1,x);
y2=evalChebyshev(c2,x);
plot(x,f(x),x,y1,x,y2)
s=sprintf('Aproxim\\u{a}ri Ceb\\^{\\i}\\c{s}ev discrete, $n=%d$',n);
legend('f','in puncte de extrem','in radacini',...
    'Location','Best')
title(s,'Interpreter','LaTeX','FontSize',14)