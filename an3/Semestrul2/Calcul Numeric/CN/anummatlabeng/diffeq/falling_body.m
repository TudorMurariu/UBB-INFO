function falling_body(y0)
opts = odeset('events',@g);
[t,y,tfinal] = ode45(@f,[0,Inf],y0,opts);
tfinal
plot(t,y(:,1),'-',[0,tfinal],[1,0],'o')
axis([-0.1, tfinal+0.1, -0.1, max(y(:,1)+0.1)]);
xlabel t
ylabel y
title('Falling body')
text(tfinal-0.8, 0, ['tfinal = ' num2str(tfinal)])
%-----
function ydot=f(t,y)
ydot = [y(2); -1+y(2)^2];
%-----
function [gstop,isterminal,direction] = g(t,y)
gstop = y(1);
isterminal = 1;
direction = 0; %[];