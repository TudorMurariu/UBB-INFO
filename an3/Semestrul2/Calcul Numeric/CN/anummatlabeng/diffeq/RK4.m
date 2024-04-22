function [t,w]=RK4(f,tspan,alfa,N)
%RK4 - classical Runge-Kutta method for equispaced nodes
%call [t,w]=RK4(f,tspan,alfa,N)
%f - right hand side function
%tspan - interval
%alfa - starting value(s)
%N - number of subintervals
%t - abscissas of solution
%w - ordinates of solution

tc=tspan(1); wc=alfa(:);
h=(tspan(end)-tspan(1))/N;
t=tc; w=wc';
for k=1:N
    K1=f(tc,wc);
    K2=f(tc+1/2*h,wc+1/2*h*K1);
    K3=f(tc+1/2*h,wc+1/2*h*K2);
    K4=f(tc+h, wc+h*K3);
    wc=wc+h/6*(K1+2*K2+2*K3+K4);
    tc=tc+h;
    t=[t;tc]; w=[w;wc'];
end