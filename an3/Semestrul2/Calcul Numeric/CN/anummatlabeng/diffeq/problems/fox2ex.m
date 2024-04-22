tspan = [0,10]; y0=[3,0];
k=1.1;
opt = odeset('RelTol',1e-6,'AbsTol',1e-6','Events',@events);
[tfox,yfox,te,ye,ie] = ode45(@fox2,tspan,y0,opt,k);
plot(yfox(:,1),yfox(:,2)), hold on
plot(sqrt(1+tfox).*cos(tfox),sqrt(1+tfox).*sin(tfox),'--')
plot([3,1],[0,0],'o')
plot(yfox(end,1),yfox(end,2),'*')
axis equal, axis([-3.5,3.5,-2.5,3.1])
legend('Fox','Rabbit',0), hold off