X=1:20
Y=4*ones(1,20)
%Y=[1:10 10:-1 :1]
xx=10.5
c=barycentricweigths(X)
barycentricInterpolation(X,Y,xx,c)

barycentricInterpolationNoFor(X,Y,xx,c)