x=rand(100,1)*16-8; y=rand(100,1)*16-8;
R=sqrt(x.^2+y.^2)+eps;
z=sin(R)./R;
xp=-8:0.5:8;
[XI,YI]=meshgrid(xp,xp);
ZI=griddata(x,y,z,XI,YI);
mesh(XI,YI,ZI); hold on
plot3(x,y,z,'ko'); hold off
