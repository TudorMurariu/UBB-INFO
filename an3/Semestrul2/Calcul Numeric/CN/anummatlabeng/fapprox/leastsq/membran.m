function [dfl,cof]=membran(h,np,ns,nx,ny)
%MEMBRAN Computes transverse deflection of a 
% uniformly tensioned membrane
% [DFL,COF]=membran(H,NP,NS,NX,NY)
% Example use: membran(0.75,100,50,40,40);
% h - width of the rectangular part
% np - number of least square points (about 3.5*np)
% ns - number of terms 
% nx,ny - the number of x points and y points
% dfl - computed array of deflection values
% cof - coefficients in the series

if nargin==0
    h=.75; np=100; ns=50; nx=40; ny=40;
end

% Generate boundary points for least square
% approximation
z=[exp(1i*linspace(0,pi/2,round(1.5*np))),...
    linspace(1i,-h+1i,np),...
    linspace(-h+1i,-h,round(np/2))];
z=z(:); xb=real(z); xb=[xb;xb(end:-1:1)];
yb=imag(z); yb=[yb;-yb(end:-1:1)]; 

% Form the least square equations and solve
% for series coefficients
a=ones(length(z),ns);
for j=2:ns, a(:,j)=a(:,j-1).*z; end
cof=real(a)\(z.*conj(z))/4;

% Generate a rectangular grid for evaluation
% of deflections
xv=linspace(-h,1,nx); yv=linspace(-1,1,ny);
[X,Y]=meshgrid(xv,yv); Z=X+1i*Y;

% Evaluate the deflection series on the grid
dfl=-Z.*conj(Z)/4+ ...
    real(polyval(cof(ns:-1:1),Z));

% Set values outside the physical region of
% interest to zero
dfl=real(dfl).*(1-((abs(Z)>=1)&(real(Z)>=0)));

% Make surface and contour plots
hold off; close; surf(X,Y,dfl);
xlabel('x axis'); ylabel('y axis');
zlabel('deflection'); view(-10,30);
title('Membrane Deflection'); colormap([1 1 1]);
shg
figure(2)
contour(X,Y,dfl,15,'k'); hold on
plot(xb,yb,'k-'); axis('equal'), hold off
xlabel('x axis'); ylabel('y axis');
title('Membrane Surface Contour Lines'), shg
