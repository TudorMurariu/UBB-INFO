x=-2:.01:2; y=-1:0.01:1;
[X,Y] = meshgrid(x,y);
Z =sin(3*Y-X.^2+1)+cos(2*Y.^2-2*X);
subplot(2,1,1)
contour(X,Y,Z,20);
subplot(2,1,2)
[C,h]=contour(X,Y,Z,[1.2,0.5,-0.4,-1.1]);
clabel(C,h)

