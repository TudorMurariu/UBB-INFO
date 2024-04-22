function s=ExpUnstable(x,tol)
% EXPUNSTABLE computation of the exponential function
% s=ExpUnstable(x,tol); computes an approximation s of exp(x)
% up to a given tolerance tol.
% WARNING: cancellation for large negative x.

if nargin<2, tol=eps; end
s=1; term=1; k=1;
while abs(term)>tol*abs(s)
    so=s; term=term*x/k;
    s=so+term; k=k+1;
end

