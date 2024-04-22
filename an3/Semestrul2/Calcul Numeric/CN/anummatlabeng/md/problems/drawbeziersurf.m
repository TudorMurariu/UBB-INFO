function drawbeziersurf(Bx,By,Bz,nx,ny)
%DRAWBEZIERSURF - deseneaza o suprafata Bezier
%apel drawbeziersurf(Bx,By,Bz,nx,ny)
%Bx,By,Bz - coordonatele punctelor de control
%nx,ny - rezolutia


u=linspace(0,1,nx);
v=linspace(0,1,ny);
[qx,qy,qz]=deCasteljau2D(Bx,By,Bz,u,v);
hh=surf(qx,qy,qz);
set(hh,'FaceAlpha',0.25,'FaceColor','green','EdgeColor','cyan')
hold on

[n,m]=size(Bx);
for i=1:n
    plot3(Bx(i,:),By(i,:),Bz(i,:),'k-o','LineWidth',2)
end
for j=1:m
    plot3(Bx(:,j),By(:,j),Bz(:,j),'k-o','LineWidth',2)
end
