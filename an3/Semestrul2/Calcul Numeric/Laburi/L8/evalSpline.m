function z=evalSpline(x,c,t)
%EVALSPLINE - compute cubic spline value
%z - result
%t - evaluation points 
%x - nodes
%c - coefficients length(x)-1 by 4 matrix
  n=length(x);
  x=x(:); t=t(:);
  k = ones(size(t));
  for j = 2:n-1
      k(x(j) <= t) = j;
  end
  % interpolant evaluation
  s = t - x(k);
  z = c(k,4) + s.*(c(k,3) + s.*(c(k,2) + s.*c(k,1)));
end
