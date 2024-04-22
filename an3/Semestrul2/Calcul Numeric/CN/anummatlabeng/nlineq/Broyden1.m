function [z,ni]=Broyden1(f,fd,x0,ea,er,nmax)
%Newton1 - Broyden method for nonlinear sytems
%call [z,ni]=Broyden1(f,x0,ea,er,nmax)
%Input
%f - function
%fd - derivative
%x0 - starting approximation 
%ea - absolute error
%er - relative error
%nmax - maximum number of iterations
%Output
%z - approximate solution
%ni - actual no. of iterations

if nargin < 6, nmax=50; end
if nargin < 5, er=0; end
if nargin < 4, ea=1e-3; end
x=zeros(length(x0),nmax+1);
F=x;
x(:,1)=x0(:);
F(:,1)=f(x(:,1));
B=inv(fd(x)); 
x(:,2)=x(:,1)+B*F(:,1);
for k=2:nmax
    F(:,k)=f(x(:,k));
    y=F(:,k)-F(:,k-1); s=x(:,k)-x(:,k-1);
    B=B+((s-B*y)*s'*B)/(s'*B*y);
    x(:,k+1)=x(:,k)-B*F(:,k);
    if norm(x(:,k+1)-x(:,k),inf)<ea+er*norm(x(:,k+1),inf)
        z=x(:,k+1); %success
        ni=k;
        return
    end
end
error('maximum iteration number exceeded')