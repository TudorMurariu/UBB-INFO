function z=evalsplinec(x,c,t)
%EVALSPLINE - compute cubic spline value
%call z=evalspline(x,c,t)
%z - values
%t - evaluation points 
%x - nodes (knots)
%c - coefficients length(x)-1 by 4 matrix
n=length(x);
x=x(:); t=t(:);
k = ones(size(t));
for j = 2:n-1
    k(x(j) <= t) = j;
end
% interpolant evaluation
s = t - x(k);
%z = d(k) + s.*(c(k) + s.*(b(k) + s.*a(k)));
z = c(k,4) + s.*(c(k,3) + s.*(c(k,2) + s.*c(k,1)));
