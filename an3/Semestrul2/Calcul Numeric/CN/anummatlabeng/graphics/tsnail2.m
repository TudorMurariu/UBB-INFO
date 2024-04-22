r=3;
s=1;
p=0.1;
k=1; d=1;
x = @(u) d*ones(size(u));
y = @(u) u;
z = @(u) k*u+1;

[X,Y,Z]=spiralsurface(x,y,z,p,[0,2*pi],[0,2*pi],50,150);
surf(X,Y,Z)
shading interp