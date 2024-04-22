function [n,erref]=excebc(f,err,proc)
%f - function
%err - error
%proc - approximation method (Legendre, continuous
%       Chebyshev, discrete Chebyshev 

x=linspace(-1,1,100); %abscissas
y=f(x); %function values
n=1;
while 1
    ycc=proc(f,x,n);  %approximation values 
    erref=norm(y-ycc,inf);  %error
    if norm(y-ycc,inf)<err  %success
        return
    end
    n=n+1;
end