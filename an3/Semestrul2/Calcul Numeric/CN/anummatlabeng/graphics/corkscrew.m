function corkscrew(a,b)
u=linspace(0,2*pi,30);
v=linspace(-pi,pi,30);
[U,V]=meshgrid(u,v);
X=a*cos(U).*cos(V);
Y=a*sin(U).*cos(V);
Z=a*sin(V)+b*U;
surf(X,Y,Z);