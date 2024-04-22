%CYCLENEWTON - cycle in Newton method
clf
fc=@(x) sign(x-2).*sqrt(abs(x-2));
x=linspace(0,4,100); y=fc(x);
plot([0,4],[0,0],'b-'); hold on
plot(x,y,'k-','LineWidth',2); 
xp=[3, 1, 1, 3];
yp=[fc(3),0,fc(1),0];
plot([xp,xp(1)],[yp,yp(1)],'g--')
plot(xp,yp,'co','MarkerSize',12)
plot(2,0,'co','MarkerSize',12)
text(3,-0.3,'x^{*}', 'FontSize', 14)
hold off
axis off