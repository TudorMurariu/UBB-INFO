[X,Y]=meshgrid(-2:0.25:2);
Z=X.^2-Y.^2;
subplot(1,3,1)
surf(X,Y,Z);
axis square
shading flat
title('shading flat','FontSize',14)

subplot(1,3,2)
surf(X,Y,Z);
axis square
shading faceted
title('shading faceted','FontSize',14)

subplot(1,3,3)
surf(X,Y,Z);
axis square
shading interp
title('shading interp','FontSize',14)

