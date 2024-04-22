%Mya arenaria
a=0.85; b=1.6; c=3.0; h=0.9; k=0;
w=2.5; umin=-1; umax=0.52;
af=2;
[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,-1,af*256,af*256); %,R,nu,nv)
surf(X,Y,Z)
view(-93,61)
shading interp
camlight right
light('Position',[-1,4,1])
axis equal
axis off