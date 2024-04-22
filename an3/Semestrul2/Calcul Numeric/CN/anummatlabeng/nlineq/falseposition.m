function [x,nit]=falseposition(f,a,b,er,nmax)
%FALSEPOSITION - false position method

if nargin < 5, nmax=100; end
if nargin < 4, er=1e-3; end
nit=0; fa=f(a); fb=f(b);
for k=1:nmax
    x=a-(a-b)*fa/(fa-fb);
    if (x-a<er*(b-a))||(b-x<er*(b-a))
        nit=k; return
    else
        fx=f(x);
        if sign(fx)==sign(fa)
            a=x; fa=fx;
        else
            b=x; fb=fx;
        end %if
    end %if
end %for
error('iteration number exceeded') 
