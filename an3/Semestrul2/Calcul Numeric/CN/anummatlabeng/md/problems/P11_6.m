function P11_6
%punctul a) - reprezentarea grafica
t=linspace(0,2*pi,30);
r=linspace(0,1,25);
[R,T]=meshgrid(r,t);
X=5*R.*cos(T);
Y=sqrt(15)*R.*sin(T);
Z=X.^2+Y.^2;
surfc(X,Y,Z)
%punctul b) - valoarea exacta
syms fe x y
fe=x^2+y^2;
ve=int(int(fe,y,-sqrt(3/5*(25-x^2)),sqrt(3/5*(25-x^2))),x,-5,5)
double(ve)
%punctul c) - cu schimbare de coordonate
f11_6=@(x,y) x.^2+y.^2;
ft=@(r,t) 5*sqrt(15)*r.*f11_6(5*r.*cos(t),sqrt(15)*r.*sin(t));
vi1=dblquad(ft,0,1,0,2*pi,1e-6)
%punctul d) - domeniu simplu in raport cu x
warning off
c=@(x) -sqrt(3/5*(25-x.^2));
d=@(x) sqrt(3/5*(25-x.^2));
vi2=quaddblsx(f11_6,-5,5,c,d,1e-6,@quadl)
warning on