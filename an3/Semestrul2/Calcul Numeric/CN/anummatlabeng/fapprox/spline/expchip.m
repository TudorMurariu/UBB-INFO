%expchip
x = -3:3; 
y = [-1 -1 -1 0 1 1 1]; 
t = -3:.01:3;
p = pchip(x,y,t);
s = spline(x,y,t);
plot(x,y,'o',t,p,'-',t,s,'-.')
legend({'data','pchip','spline'},4)