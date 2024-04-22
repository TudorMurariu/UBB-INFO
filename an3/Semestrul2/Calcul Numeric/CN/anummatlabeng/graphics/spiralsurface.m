function [X,Y,Z]=spiralsurface(x,y,z,p,iu,it,nu,nt)
%SPIRALSURFACE - generate a spiral surface
%x,y,z -  x(u),y(u), z(u) parametrization of generator
%         curve
%p - parameter
%iu - interval for u
%it - interval for t
%nu - #points
%nt - #points

u=linspace(iu(1),iu(2),nu);
t=linspace(it(1),iu(2),nt);
[U,T]=meshgrid(u,t);
ept=exp(p*T);
X=ept.*(x(U).*cos(T)-y(U).*sin(T));
Y=ept.*(x(U).*sin(T)+y(U).*cos(T));
Z=ept.*z(U);