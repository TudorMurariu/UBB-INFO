function y=Legendreapprox(f,x,n)
%LEGENDREAPPROX - continuous least squares aproximare continua mcmmp Legendre
%call y=Legendreapprox(f,x,n)
%f - function
%x - points
%n - degree

c=Legendrecoeff(f,n);
y=evalLegendreapprox(c,x);
