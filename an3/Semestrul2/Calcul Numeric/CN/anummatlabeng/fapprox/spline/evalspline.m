function z=evalspline(x,a,b,c,d,t)
%EVALSPLINE - compute cubic spline value
%call z=evalspline(x,a,b,c,d,t)
%z - values
%t - evaluation points 
%x - nodes (knots)
%a,b,c,d - coefficients
n=length(x);
x=x(:); t=t(:);
k = ones(size(t));
for j = 2:n-1
    k(x(j) <= t) = j;
end
% interpolant evaluation
s = t - x(k);
z = d(k) + s.*(c(k) + s.*(b(k) + s.*a(k)));
