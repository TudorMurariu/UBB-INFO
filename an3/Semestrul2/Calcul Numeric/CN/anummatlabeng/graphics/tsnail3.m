r=3;
s=1;
p=0.1;
k=1; d=1;
x = @(u) 4.2-3*sin(u);
y = @(u) -3*cos(u);
z = @(u) sin(u)+cos(u);

[X,Y,Z]=spiralsurface(x,y,z,p,[0,2*pi],[0,2*pi],50,150);
surf(X,Y,Z)
shading interp