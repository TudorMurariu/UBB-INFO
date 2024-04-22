function [x,y,nfev]=Runge_Kutta(f,tspan,y0,N,BT)
%RUNGE_KUTTA - Runge-Kutta method with constant step
%call [t,y,nfev]=Runge_Kutta(f,tspan,y0,N,BT)
%f -right-hand side function
%tspan - interval [a,b]
%y0 - starting value(s)
%N - number of steps
%BT - function that provides Butcher table, call syntax
%        [lambda,alfa,mu,s] - s number of stages
%t -abscissas of solution
%y - ordonates of solution components
%nfev - number of function evaluation

[lambda,alfa,mu,r]=BT(); %initialize Butcher table
h=(tspan(end)-tspan(1))/N; %step length
xc=tspan(1); yc=y0(:); 
x=xc; y=yc';
K=zeros(length(y0),r);
for k=1:N %RK iteration
    K(:,1)=f(xc,yc);
    for i=2:r
        K(:,i)=f(xc+mu(i)*h,yc+h*(K(:,1:i-1)*lambda(i,1:i-1)'));
    end
    yc=yc+h*(K*alfa);
    xc=xc+h; %prepare next iteration
    x=[x;xc]; y=[y;yc'];
end
if nargout==3
    nfev=r*N;
end
