
y0=[2;0];
[t,w]=ode45(@vanderPol,[0,20],y0,[],1);
[t2,w2]=ode15s(@vanderPol,[0,3000],y0,[],1000);
plot(t,w(:,1),'-',t,w(:,2),'--')
legend('y_1','y_2')
title('Ecuatia lui van der Pol, \mu=1','FontSize',16)
figure(2)
plot(t2,w2(:,1))
title('Ecuatia lui van der Pol, \mu=1000','FontSize',16)