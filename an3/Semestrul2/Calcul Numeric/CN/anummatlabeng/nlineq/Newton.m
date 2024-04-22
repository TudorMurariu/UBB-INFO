function [z,ni]=Newton(f,fd,x0,ea,er,nmax)
%NEWTON - Newton method for nonlinear equations in R and R^n
%call [z,ni]=Newton(f,fd,x0,ea,er,nmax)
%Input
%f - function
%fd - derivative
%x0 - starting value
%ea,er - absote and relative error, respectively
%nmax - maximum number of iterations
%Output
%z - approximate solution
%ni - actual no. of iterations


if nargin < 6, nmax=50; end
if nargin < 5, er=0; end
if nargin < 4, ea=1e-3; end
xp=x0(:);   %previous x
for k=1:nmax
    xc=xp-fd(xp)\f(xp); 
    if norm(xc-xp,inf)<ea+er*norm(xc,inf)
        z=xc; %success
        ni=k;
        return
    end
    xp=xc;
end
error('maximum iteration number exceeded')
