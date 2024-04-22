tspan = [0,10]; y0=[3,0];
k=0.75;
[tfox,yfox] = ode45(@fox1,tspan,y0,[],k);
plot(yfox(:,1),yfox(:,2)), hold on
plot(sqrt(1+tfox).*cos(tfox),sqrt(1+tfox).*sin(tfox),'--')
plot([3,1],[0,0],'o')
axis equal, axis([-3.5,3.5,-2.5,3.1])
legend('Fox','Rabbit',0), hold off