function y=approxChebyshevdiscr2(f,x,n)
%APPROXCHEBYSHEVDISCR2 - aproximare discreta mcmmp Cebisev #1
%cu punctele de extrem
%apel y=approxChebyshevdiscr(f,x,n)
%f - functia
%x - punctele
%n - gradul

c=coeffChebyshevdiscr2(f,n);
y=evalChebyshev(c,x);