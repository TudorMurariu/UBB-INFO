%Natica stelata
clf
a=2.6; b=2.4; c=1.0; h=1.25; k=-2.8;
w=0.18; umin=-20; umax=1.0;
af=2;
[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,-1,af*256,af*256); %,R,nu,nv)
surf(X,Y,Z)
shading interp
view(-20,34)
camlight left
light('Position',[-2,3,1])
axis off