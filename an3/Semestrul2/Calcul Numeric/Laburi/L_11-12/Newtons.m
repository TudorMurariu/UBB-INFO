﻿function [z,ni]=Newtons(f,fd,x0,ea,er,Nmax)
%NEWTONS - Newton method in R
%Input
%f - function
%fd - derivative
%x0 - starting value
%ea,er - absolute and relative error, respectively
%Nmax - maximum number of iterations
%Output
%z - aproximate solution
%ni - actual no. of iterations

if nargin<6, Nmax=50; end
if nargin<5, er=0; end
if nargin<4, ea=1e-3; end
xv=x0;
for k=1:Nmax
    xc=xv-f(xv)/fd(xv);
    if abs(xc-xv)<ea+er*abs(xc) %success
        z=xc;
        ni=k;
        return
    end
    xv=xc; %prepare next iteration
end
%failure
error('maximum iteration number exceeded')
        