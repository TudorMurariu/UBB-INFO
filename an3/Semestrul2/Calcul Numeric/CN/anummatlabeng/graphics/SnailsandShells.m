function [X,Y,Z]=SnailsandShells(a,b,c,h,k,w,umin,umax,R,nu,nv)
%SNAILSANDSHELLS - plot snails & shell surface
%call SNAILSANDSHELLS(A,B,C,H,K,W,R,UMIN,UMAX]
%R =+1/-1 direction
%a, b, c, h, k, w - shape parameters
%umin, umax - interval for u
%nu,nv - number of points

if nargin<11, nv=100; end
if nargin<10, nu=100; end
if nargin<9, R=1; end
v=linspace(0,2*pi,nv);
u=linspace(umin,umax,nu);
[U,V]=meshgrid(u,v);
ewu=exp(w*U);
X=(h+a*cos(V)).*ewu.*cos(c*U);
Y=R*(h+a*cos(V)).*ewu.*sin(c*U);
Z=(k+b*sin(V)).*ewu;
