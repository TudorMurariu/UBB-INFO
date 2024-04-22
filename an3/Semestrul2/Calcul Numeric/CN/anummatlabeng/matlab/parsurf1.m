u=linspace(0,2*pi,40);
[U,V]=meshgrid(u);
%x = u(3 + cos(v)) cos(2u), y = u(3 + cos(v)) sin(2u), z = u sin(v) ? 3u
X = U.*(3+cos(V)).*cos(2*U);
Y = U.*(3+cos(V)).*sin(2*U);
Z = U.*sin(V)-3*U;
surf(X,Y,Z)