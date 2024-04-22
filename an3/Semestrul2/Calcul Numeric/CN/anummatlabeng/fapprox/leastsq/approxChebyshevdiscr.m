function y=approxChebyshevdiscr(f,x,n)
%APPROXCHEBYSHEVDISCR - aproximare continua mcmmp Cebisev #1
%apel y=approxChebyshevdiscr(f,x,n)
%f - functia
%x - punctele
%n - gradul

c=coeffChebyshevdiscr(f,n);
y=evalChebyshev(c,x);