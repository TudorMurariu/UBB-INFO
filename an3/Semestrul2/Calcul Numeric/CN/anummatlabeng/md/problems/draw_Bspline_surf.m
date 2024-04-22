function draw_Bspline_surf(Bx,By,Bz,gx,gy,nx,ny)
%DRAW_BSPLINE_SURF - deseneaza o suprafata B-spline
%apel draw_Bspline_surf(Bx,By,Bz,gx,gy,nx,ny)
%Bx,By,Bz - coordonatele punctelor de control
%gx,gy - gradele
%nx,ny - rezolutia

[n,m]=size(Bx);
u=noduri(n-1,gx);
v=noduri(m-1,gy);
lx=length(u); ly=length(v);
pasx=u(lx)/nx; pasy=v(ly)/ny;
s=0:pasx:u(lx);
t=0:pasy:v(ly);

[qx,qy,qz]=Cox_deBoor_2D(gx,gy,u,v,Bx,By,Bz,s,t);
hh=surf(qx,qy,qz);
set(hh,'FaceAlpha',0.35,'FaceColor','green','EdgeColor','cyan',...
    'FaceLighting','phong','AmbientStrength',0.5)
light('Position',[1 0 1],'Style','infinite');
hold on


for i=1:n
    plot3(Bx(i,:),By(i,:),Bz(i,:),'k-o','LineWidth',2)
end
for j=1:m
    plot3(Bx(:,j),By(:,j),Bz(:,j),'k-o','LineWidth',2)
end
view(-114,35)
axis off