subplot(211)
ezcontour('sin(3*y-x^2+1)+cos(2*y^2-2*x)',[-2,2,-1,1]);
%
x=-2:.01:2; y=-1:0.01:1;
[X,Y] = meshgrid(x,y);
Z =sin(3*Y-X.^2+1)+cos(2*Y.^2-2*X);
subplot(212)
contour(x,y,Z,20)
