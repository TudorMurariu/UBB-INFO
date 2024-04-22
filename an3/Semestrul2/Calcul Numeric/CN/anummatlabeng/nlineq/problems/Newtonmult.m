function [z,ni]=Newtonmult(f,fd,x0,ea,er,Nmax)
%NEWTONmult - Newton method for multiple roots
%call [z,ni]=Newtonmult(f,fd,x0,ea,er,Nmax)
%Input
%f - function
%fd - derivative
%x0 - starting value
%ea,er - absolute and relative error, respectively
%Nmax - maximum no. of iteration
%Output
%z - approximate solution
%ni - actual iteration count

if nargin<6, Nmax=50; end
if nargin<5, er=0; end
if nargin<4, ea=1e-3; end
xk2=x0; fk2=f(xk2); fdk2=fd(xk2);
xk1=xk2-fk2/fdk2; %find an approximation
fk1=f(xk1); fdk1=fd(xk1);
xk=xk1-fk1/fdk1;
fk=f(xk); fdk=fd(xk);
mp=1;
for k=1:Nmax
    %estimate multiplicity
    m=log(abs(fk1/fk2))/log(abs((xk1-xk)/(xk2-xk)))
    if abs(mp-m)>1, m=mp; end
    %find new approximation
    xn=xk-m*fk/fdk;
    fn=f(xn); fdn=fd(xn);
    if abs(xn-xk)<ea+er*abs(xn) %success
        z=xn;
        ni=k;
        return
    end
    %prepare next iteration
    xk2=xk1; fk2=fk1; fdk2=fdk1;
    xk1=xk; fk1=fk; fdk1=fdk;
    xk=xn; fk=fn; fdk=fdn;
    mp=m;
end
%failure
error('maximum #iteration exceeded')
        
