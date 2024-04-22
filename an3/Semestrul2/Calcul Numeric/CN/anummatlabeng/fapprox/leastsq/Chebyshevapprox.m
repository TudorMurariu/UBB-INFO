function y=Chebyshevapprox(f,x,n)
%CHEBYSHEVAPPROX - continuous least square Cebisev #1 approx
%call y=Chebyshevapprox(f,x,n)
%f - function
%x - points
%n - degree
%y - approximation value

c=Chebyshevcoeff(f,n);
y=evalChebyshev(c,x);