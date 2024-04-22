function y=expss(x)
%EXPSS - exponential by scaling and squaring method
%call Y=EXPSS(x)

s=sign(x);
[F,E] = log2(abs(x));
E = E+1; F = F/2;
y=expTaylor(F);
y=y.^(2.^E);
y(s<0)=1./y(s<0);

function y=expTaylor(x)
%EXPTAYLOR - exponential by Taylor polynomial with 16 terms
c=1./cumprod([1, 1:14]);
y=polyval(c(end:-1:1),x);