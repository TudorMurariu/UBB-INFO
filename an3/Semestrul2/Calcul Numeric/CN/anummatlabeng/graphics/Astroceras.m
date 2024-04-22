%Astroceras
a=1.25; b=1.25; c=1.0; h=3.5; k=0;
w=0.12; umin=-40; umax=-1;
af=2;
[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,-1,af*256,af*256); %,R,nu,nv)
surf(X,Y,Z)
shading interp
view(43,38)
camlight right
axis off