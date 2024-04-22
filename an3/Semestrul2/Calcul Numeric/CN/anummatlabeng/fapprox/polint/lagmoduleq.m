function lagmoduleq(n)
%test convergenta interpolare Lagrange 
%pentru noduri echidistante - |x|
k=1:n;
xn=-1:2/n:1;
yn=abs(xn);
xg=-1:0.04:1;
yg=abs(xg);
ta=-1:2/(n*150):1;
ya=lagr(xn,yn,ta);
plot(xg,yg,':',ta,ya,'k-');
