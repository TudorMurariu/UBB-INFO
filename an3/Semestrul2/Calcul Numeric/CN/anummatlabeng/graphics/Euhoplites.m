%Euhoplites
a=0.6; b=0.4; c=1.0; h=0.9; k=0.0;
w=0.1626; umin=-40; umax=-1;
af=2;
[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,-1,af*256,af*256); %,R,nu,nv)
surf(X,Y,Z)
view(78,46)
shading interp
camlight right
light('Position',[1,1,-0.1])
axis off