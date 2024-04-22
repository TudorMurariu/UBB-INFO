function cimplicit(a,b)
x=linspace(0,a,31);
y=linspace(-2/b,2/b,31);
[X,Y]=meshgrid(x,y);
Z=b^2*Y.^2-X.^3.*(a-X);
contour(X,Y,Z,[0,0])