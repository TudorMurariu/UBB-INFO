function y = fd_deriv(f,x,h)
%FD_DERIV   Aproximates derivative by divided difference 
%           FD_DERIV(F,X,H) is the divided diference of F
%           with nodes X and X+H. 
%           default H.  SQRT(EPS).

if nargin < 3, h = sqrt(eps); end
f=fcnchk(f);
y = (f(x+h) - f(x))/h;
