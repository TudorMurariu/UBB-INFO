function s=sinTaylor(x)
  s=0;
  t=x;
  n=1;
  while s+t~=s
    s=s+t;
    t=t*(-1)*(x^2)/((n+1)*(n+2));
    n=n+2;
  end
end