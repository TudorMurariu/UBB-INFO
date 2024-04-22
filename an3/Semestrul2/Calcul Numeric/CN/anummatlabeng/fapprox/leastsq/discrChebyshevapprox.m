function y=discrChebyshevapprox(f,x,n)
%DISCRCHEBYSHEVAPPROX - discrete least square Chebyshev #1
%call y=discrChebyshevapprox(f,x,n)
%f - function
%x - points
%n - degree

c=discrChebyshevcoeff(f,n);
y=evalChebyshev(c,x);