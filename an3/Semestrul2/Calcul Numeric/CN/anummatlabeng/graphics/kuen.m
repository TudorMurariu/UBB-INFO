function kuen(d)
u=linspace(-d,d,500);
v=linspace(0,pi,100);
[U,V]=meshgrid(u,v);
X=2*(cos(U)+U.*sin(U)).*sin(V)./(1+U.^2.*(sin(V)).^2);
Y=2*(sin(U)-U.*cos(U)).*sin(V)./(1+U.^2.*(sin(V)).^2);
Z=log(tan(V/2))+2*cos(V)./(1+U.^2.*(sin(V)).^2);
surf(X,Y,Z)
shading interp