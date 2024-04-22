%start a figure and set up the mesh
figure(1) ; clf
t = linspace(0, 2*pi, 512) ;
[u,v] = meshgrid(t) ;
%Next, define the surface
a = -0.2 ; b = .5 ; c = .1 ;
n = 2 ;
x = (a*(1-v/(2*pi)).*(1+cos(u)) + c) ...
.* cos(n*v) ;
y = (a*(1-v/(2*pi)).*(1+cos(u)) + c) ...
.* sin(n*v) ;
z = b*v/(2*pi) + ...
a*(1-v/(2*pi)) .* sin(u) ;
%plot the surface
h=surf(x,y,z,y);
view(150,28);
shading interp
light('Position',[-1 1 1],'Style','infinite');
lighting  phong
