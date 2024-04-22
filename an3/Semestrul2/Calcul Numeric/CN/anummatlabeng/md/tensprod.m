function Z=tensprod(u,v,x,y,f)
%TENSPROD - tensor product Lagrange interpolant
%call [Z,X,Y]=TENSPROD(U,V,X,Y,F)
%U - evaluation abscissas
%V - evaluation ordinates
%X - node abscissas
%Y - node ordinates 
%F - function

[X,Y]=meshgrid(x,y);
F=f(X,Y);;
lu=pfl2b(x,u)';
lv=pfl2b(y,v);
Z=lu*F*lv;
