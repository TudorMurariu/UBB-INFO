function c=discrChebyshevcoeff(f,n)
%DISCRCHEBYSHEVCOEFF - discrete least squares Chebyshev coefficients
%call c=discrChebyshevcoeff(f,n)
%f - function
%n - degree

xi=cos((2*[1:n+1]-1)*pi/(2*n+2));
y=f(xi)';
for k=1:n+1
    c(k)=2/(n+1)*vChebyshev(xi,k-1)*y;
end