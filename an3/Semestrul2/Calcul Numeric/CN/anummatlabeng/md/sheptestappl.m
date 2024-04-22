ftest=@(x,y) x.*exp(-x.^2-y.^2);
nX=[4*rand(203,1)-2];
nY=[4*rand(203,1)-2];
nX=nX(:); nY=nY(:); f=ftest(nX,nY);
[X,Y]=meshgrid(linspace(-2,2,103));
Z2a=Shepgridbloc(X,Y,nX,nY,f,2,0.4);
Z3a=Shepgridbloc(X,Y,nX,nY,f,3,0.4);
Z2b=Shepgridbloc(X,Y,nX,nY,f,2,0.2);
Z3b=Shepgridbloc(X,Y,nX,nY,f,3,0.2);
subplot(2,2,1); surf(X,Y,Z2a)
title('\mu=2, R=0.3','FontSize',14)
shading interp; camlight headlight
subplot(2,2,2); surf(X,Y,Z3a)
title('\mu=3, R=0.3','FontSize',14)
shading interp; camlight headlight
subplot(2,2,3); surf(X,Y,Z2a)
title('\mu=2, R=0.2','FontSize',14)
shading interp; camlight headlight
subplot(2,2,4); surf(X,Y,Z3a)
title('\mu=3, R=0.2','FontSize',14)
shading interp; camlight headlight
