function Z=RBF(X,Y,nX,nY,f,phi)
%RBF - radial basis function interpolant
%call Z=RBF(X,Y,nX,nY,f,phi)
%X,Y - points for evaluation
%nX,nY - nodes
%f - funcntion values at nodes
%phi - radial basis function

if nargin<6
    phi=@(x) (x+1).^(1/2);
end
D=sqdist(nX,nY); %compute square of distances
%find coefficients
A=phi(D);
a=A\f;
%compute interpolant values
n=length(nX);
Z=zeros(size(X));
for j=1:n
    Z=Z+a(j)*phi((X-nX(j)).^2+(Y-nY(j)).^2);
end
function M=sqdist(X,Y)
%compute squares of distances
[rX1,rX2]=meshgrid(X);
[rY1,rY2]=meshgrid(Y);
M=(rX1-rX2).^2+(rY1-rY2).^2;
