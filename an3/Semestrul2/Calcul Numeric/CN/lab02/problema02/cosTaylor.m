function c=cosTaylor(x)
  c=0;
  t=1;
  n=1;
  while c+t~=c
    c=c+t;
    t=t*(-1)*(x^2)/(n*(n+1));
    n=n+2;
  end
end