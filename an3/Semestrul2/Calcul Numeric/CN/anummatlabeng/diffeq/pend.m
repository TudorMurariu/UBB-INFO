function yp=pend(t,y,g,L)
%PEND - simple pendulum
%g - acceleration due to gravity, L - length
yp=[y(2); -g/L*sin(y(1))];