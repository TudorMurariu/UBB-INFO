function Z=boolsum(u,v,x,y,f)
%BOOLSUM - interpolant Lagrange suma booleana
%call [Z,X,Y]=BOOLSUM(U,V,X,Y,F)
%U - evaluation abscissas
%V - evaluation ordinates
%X - node abscissas
%Y - node ordinates 
%F - function

[X,Y]=meshgrid(x,y);
F=f(X,Y);
[X1,V1]=meshgrid(x,v);
F1=f(X1,V1);
[U2,Y2]=meshgrid(u,y);
F2=f(U2,Y2);
lu=pfl2b(x,u);
lv=pfl2b(y,v);
Z=F1*lu+lv'*F2-lu'*F*lv;
