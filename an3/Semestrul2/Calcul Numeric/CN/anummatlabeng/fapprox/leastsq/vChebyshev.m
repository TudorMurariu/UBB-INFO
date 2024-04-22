function y=vChebyshev(x,n)
%VCHEBYSHEV - values of Chebyshev #1 polynomial
%call y=vChebyshev(x,n)
%x - points
%n - degree
%y - values of Chebyshev polynomial

pnm1=ones(size(x));
if n==0, y=pnm1; return; end
pn=x;
if n==1, y=pn; return; end
for k=2:n
    y=2*x.*pn-pnm1;
    pnm1=pn;
    pn=y;
end