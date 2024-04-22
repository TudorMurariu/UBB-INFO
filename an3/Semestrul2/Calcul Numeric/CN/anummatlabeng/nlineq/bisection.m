function [x,k]=bisection(f,a,b,err)
%BISECTION - bisection method

if nargin<3, err=eps; end
k = 0;
while abs(b-a) > err*abs(b)
    x = (a + b)/2;
    if sign(f(x)) == sign(f(b))
        b = x;
    else
        a = x;
    end
    k = k + 1;
end