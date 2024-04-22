%if you are courious uncomment the middle part
clf
t = linspace(0, 2*pi, 512) ;
[U,V] = meshgrid(t) ;
%a=0.2, 0.5,-0.5
a = 0.5 ; b = .5 ; c = .1 ;
n = 2 ;
x = (a*(1-V/(2*pi)).*(1+cos(U)) + c).* cos(n*V) ;
y = (a*(1-V/(2*pi)).*(1+cos(U)) + c).* sin(n*V) ;
z = b*V/(2*pi) + a*(1-V/(2*pi)) .* sin(U) ;
surf(x,y,z,y)
shading interp
% hold on
% xx=linspace(-1,1.5,20);
% yy=linspace(-1,1.5,20);
% zz=linspace(-0.7,0.7,20);
% [X,Y]=meshgrid(xx,yy);
% h2=surf(X,Y,-0.7*ones(size(X)));
% set(h2,'FaceColor',[0.7,0.7,0.7],'EdgeColor','none')
% [X,Z]=meshgrid(xx,zz);
% h3=surf(X,1.5*ones(size(X)),Z);
% set(h3,'FaceColor',[0.7,0.7,0.7],'EdgeColor','none')
view(7,42) %for a=0.5
%view(-136,28) %for a=-0.5
light('Position',[0,-1,0.5])
light('Position',[0.5,0,0.5],'Style','local')
axis off