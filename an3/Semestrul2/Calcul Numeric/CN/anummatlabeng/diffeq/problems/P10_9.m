function P10_10
%P10_10 problema stiff

[t,w]=ode45(@ecstiff,[1,10],[1,1]);
[t2,w2]=ode15s(@ecstiff,[1,10],[1,1]);
whos
plot(t,w,t2,w2,t2,1./t2.^2,t2,1./sqrt(t2))
function yd=ecstiff(x,y)
yd=[1/y(1)-x^2-2/x^3; y(1)/y(2)^2-1/x-1/(2*x^(3/2))];