%Pseudoheliceras subcatenatum
a=1.6; b=1.6;c=1; h=1.5; k=-7.0;
w=0.075; umin=-50; umax=-1;
af=4;

[X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,1,af*256,af*256); 
surf(X,Y,Z)
view(87,21)
shading interp
camlight right
light('Position',[3,-0.5,-4])
axis off
