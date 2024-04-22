function c=taylor_coef(f,n,a=0)
  if n<0
    c=sym(0);
    return;
  else
    c=subs(diff(f, n), a)/factorial(n);
  end
endfunction