X=1:20;
Y=4*ones(1,20);
xx=10.5;
c=barycentricweigths(X);
barycentricInterpolation(X,Y,xx,c);

c=barycentricweigths([3 4 5]);
barycentricInterpolation([3 4 5], [7 6 3], 3.8, c)

Lagrange([3 4 5], [7 6 3], 3.8) 