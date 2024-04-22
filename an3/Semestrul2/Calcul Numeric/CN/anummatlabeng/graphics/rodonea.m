function rodonea(a,n,lp)
if nargin<3, lp=2; end
t=linspace(0,lp*pi,200);
x=a*sin(n*t).*cos(t);
y=a*sin(n*t).*sin(t);
z=a*cos(n*t);
plot3(x,y,z);