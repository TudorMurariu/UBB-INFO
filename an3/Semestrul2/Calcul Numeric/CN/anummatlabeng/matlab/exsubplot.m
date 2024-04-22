x = linspace(0,15,100);
subplot(2,2,1), plot(x,sin(x))
subplot(2,2,2), plot(x,round(x))
%subplot(2,1,2), plot(x,sin(round(x)))
subplot(2,2,3:4), plot(x,sin(round(x)))