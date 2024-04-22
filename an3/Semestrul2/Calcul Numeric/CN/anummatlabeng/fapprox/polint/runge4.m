%function runge4(n)
%test convergenta interpolare Lagrange pentru noduri Cebisev
k=1:n;
xn=sort(cos((2*k-1)*pi/2/n));
yn=abs(xn);
xg=-1:0.04:1;
yg=abs(xg);
ta=-1:0.01:1;
ya=lagr2(xn,yn,ta);
plot(xg,yg,'--',ta,ya);
