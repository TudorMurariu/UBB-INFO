[X,Y]=meshgrid(-3:1:3);
Z=peaks(X,Y);
figure(1)
surf(X,Y,Z)
[XI,YI]=meshgrid(-3:0.25:3);
ZI1=interp2(X,Y,Z,XI,YI,'nearest');
ZI2=interp2(X,Y,Z,XI,YI,'linear');
ZI3=interp2(X,Y,Z,XI,YI,'cubic');
ZI4=interp2(X,Y,Z,XI,YI,'spline');
figure(2)
subplot(2,2,1)
surf(XI,YI,ZI1)
title('nearest')
subplot(2,2,2)
surf(XI,YI,ZI2)
title('linear')
subplot(2,2,3)
surf(XI,YI,ZI3)
title('cubic')
subplot(2,2,4)
surf(XI,YI,ZI4)
title('spline')
