function elicoid(a,b,c,d,e)
u=linspace(0,2*pi,31);
v=linspace(-d,d,31);
[U,V]=meshgrid(u,v);
X=a*V.*cos(U);
Y=b*V.*sin(U);
Z=c*U+e*V;
surf(X,Y,Z);