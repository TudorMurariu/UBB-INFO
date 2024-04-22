%P5_5 -functia lui Lebesgue
n=input('n=');
t=linspace(0,1,50*n);
%noduri echidistante
x1=linspace(0,1,n);
Lu=Lebesgue(x1,t);
k=1:n;
x2=cos((2*k-1)*pi/(2*n));
x2=(x2+1)/2;
Lc=Lebesgue(x2,t);
subplot(2,1,1)
plot(t,Lu)
s=sprintf('noduri echidistante, n=%d',n);
title(s)
subplot(2,1,2)
plot(t,Lc);
s=sprintf('noduri Cebisev, n=%d',n);
title(s)