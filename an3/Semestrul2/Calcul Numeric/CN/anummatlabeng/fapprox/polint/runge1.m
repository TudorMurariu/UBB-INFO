%function runge1(n)
%test convergenta interpolare Lagrange pentru noduri Cebisev
k=1:n;
xn=sort(cos((2*k-1)*pi/2/n));
yn=(1+25*xn.^2).^(-1);
xg=-1:0.04:1;
yg=(1+25*xg.^2).^(-1);
disp('*****')
pause
ta=-1:0.01:1;
ya=lagr2(xn,yn,ta);
plot(xn,yn,'o',xg,yg,ta,ya,'-.');
