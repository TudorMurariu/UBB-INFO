t = 0:.1:2*pi;
subplot(2,2,1)
plot(cos(t),t.*sin(t),'k-')
subplot(2,2,2)
plot(cos(t),sin(2*t),'k-')
subplot(2,2,3)
plot(cos(t),sin(3*t),'k-')
subplot(2,2,4)
plot(cos(t),sin(4*t),'k-')