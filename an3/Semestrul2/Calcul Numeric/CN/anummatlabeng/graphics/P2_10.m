figure(1) ; clf
t = linspace(0, 2*pi, 512) ;
[U,V] = meshgrid(t) ;
X=U.*(3+cos(V)).*cos(2*U);
Y=U.*(3+cos(V)).*sin(2*U);
% X=U.*(3+cos(V)).*cos(1.5*U);
% Y=U.*(3+cos(V)).*sin(1.5*U);
Z=U.*sin(V)-3*U;
surf(X,Y,Z)
shading interp
view(134,14)
camlight headlight
axis off
