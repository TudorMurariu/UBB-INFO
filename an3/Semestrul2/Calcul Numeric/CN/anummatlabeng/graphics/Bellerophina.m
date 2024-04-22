%Bellerophina
a=0.85; b=1.2; c=1.0; h=0.75; k=0.0;
w=0.06; umin=-10; umax=-1;
af=2;

[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,1,af*256,af*256); %,R,nu,nv)
surf(X,Y,Z)
shading interp
view(76,20)
camlight right
axis off