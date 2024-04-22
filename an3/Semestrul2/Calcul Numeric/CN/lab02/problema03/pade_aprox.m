function R=pade_aprox(f,m,k,x)
  if k==0
    R=taylor(f,x,0,'order',m+1);
  else
    C=sym(zeros(k,k));
    for i=1:k
      for j=1:k
        C(i,j)=taylor_coef(f,m+i-j);
      endfor
    endfor
    d=sym(zeros(k,1));
    for i=1:k
      d(i)=-taylor_coef(f,m+i);
    endfor
    b=inv(C)*d;
    b=[1; b];
    a=sym(zeros(m+1));
    for j=1:m+1
      for l=1:j+1
        if l<=length(b)
          a(j)=a(j)+taylor_coef(f,j-l)*b(l);
        endif
      endfor
    endfor
    P=sym(0);
    for i=0:m
      P=P+a(i+1)*(x^i);
    endfor
    Q=sym(0);
    for i=0:k
      Q=Q+b(i+1)*(x^i);
    endfor
    R=P/Q;
  endif
endfunction