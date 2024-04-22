%Nautilus
a=1; b=0.6; c=1; h=1; k=0;
w=0.18; umin=-20; umax=1;
af=2;
clf
[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,1,af*256,af*256);
surf(X,Y,Z,Y)
view(-75,32)
shading interp
camlight headlight
axis off