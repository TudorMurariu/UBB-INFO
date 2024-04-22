r=35;
s=25;
p=0.4;
x = @(u) s+s*cos(u);
y = @(u) zeros(size(u));
z = @(u) r*sin(u);

[X,Y,Z]=spiralsurface(x,y,z,p,[0,2*pi],[0,2*pi],50,50);
surf(X,Y,Z)