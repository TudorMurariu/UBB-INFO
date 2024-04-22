%function runge2(n)
%test convergenta interpolare Lagrange 
%pentru noduri echidistante
k=1:n;
xn=-1:2/n:1;
yn=(1+25*xn.^2).^(-1);
xg=-1:0.04:1;
yg=(1+25*xg.^2).^(-1);
disp('*****')
pause
ta=[-1:0.002:-0.5,-0.5:0.032:0.5, 0.5:0.002:1];
ya=lagr2(xn,yn,ta);
plot(xn,yn,'o',xg,yg,ta,ya,'-.');
