function para(a,b)
x=linspace(0,a,151);
la=max(sqrt(x.^3.*(a-x)/b));
y=linspace(-la,la,151);
[X,Y]=meshgrid(x,y);
Z=b^2*Y.^2-X.^3.*(a-X);
contour(X,Y,Z,[0,0])