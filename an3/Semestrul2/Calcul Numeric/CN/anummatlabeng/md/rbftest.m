%RBF TEST
ftest=@(x,y) x.*exp(-x.^2-y.^2);
phi=@(r) exp(-2*r);
%phi=@(r)((1-sqrt(r)).^4>=0).*(1-sqrt(r)).^4.*(1+4*sqrt(r));
%phi=@(r)(1+r).^(-1/2);
%phi = @(r) r.*log(sqrt(r)+100*eps);
%phi =@(r) r.^2;

nX=[4*rand(100,1)-2;-2;-2;2;2];
nY=[4*rand(100,1)-2;-2;2;-2;2];
[X,Y]=meshgrid(linspace(-2,2,40));
nX=nX(:); nY=nY(:);
f=ftest(nX,nY);
Z1=RBF(X(:),Y(:),nX,nY,f,phi);
Z2=RBF(X(:),Y(:),nX,nY,f);
ZE=ftest(X,Y);
T=delaunay(X(:),Y(:));
G1=del2(Z1);
G2=del2(Z2);
figure(1)
trisurf(T,X,Y,Z1,G1)
figure(2)
trisurf(T,X,Y,abs(Z1-ZE(:)),G1)
figure(3)
trisurf(T,X,Y,Z2,G2)
figure(4)
trisurf(T,X,Y,abs(Z2-ZE(:)),G2)