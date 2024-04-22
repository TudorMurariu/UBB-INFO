[X,Y] = meshgrid(linspace(0,2*pi,30),linspace(0,pi,30));
Z = sin(X).*cos(Y);
surf(X,Y,Z)
xlabel('x'); ylabel('y'); zlabel('z');
axis([0 2*pi 0 pi -1 1])