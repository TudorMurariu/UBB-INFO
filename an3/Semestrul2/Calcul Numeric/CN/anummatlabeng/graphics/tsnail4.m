a=2;
b=2;
p=0.4;
k=1; d=1;
x = @(u) a*u.*cos(u);
y = @(u) a*u.*sin(u);
z = @(u) b*u;

[X,Y,Z]=spiralsurface(x,y,z,p,[0,2*pi],[0,2*pi],50,150);
surf(X,Y,Z)
shading interp