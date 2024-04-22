ftest=@(x,y) x.*exp(-x.^2-y.^2);
nX=[4*rand(201,1)-2];
nY=[4*rand(201,1)-2];
nX=nX(:); nY=nY(:); f=ftest(nX,nY);
[X,Y]=meshgrid(linspace(-2,2,113));
Z2a=Shepgridbloc(X,Y,nX,nY,f,2,0.4);
Z3a=Shepgridbloc(X,Y,nX,nY,f,3,0.4);
surf(X,Y,Z2a)
title('\mu=2, R=0.4','FontSize',14)
shading interp; camlight headlight
figure(2)
surf(X,Y,Z3a)
title('\mu=3, R=0.4','FontSize',14)
shading interp; camlight headlight

