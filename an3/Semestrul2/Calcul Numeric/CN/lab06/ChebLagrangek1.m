function ff=ChebLagrangek1(y,xx,a,b)
%CHEBLAGRANGEK1 - Lagrange interpolation for Chebyshev #1 points- barycentric
%call ff=ChebLagrangek1(y,xx,a,b)
%y - function values;
%xx - evaluation points
%a,b - interval
%ff - values of Lagrange interpolation polynomial

n = length(y)-1;
if nargin==2
    a=-1; b=1;
end
c = sin((2*(0:n)'+1)*pi/(2*n+2)).*(-1).^((0:n)');
x = sort(cos((2*(0:n)'+1)*pi/(2*n+2))*(b-a)/2+(a+b)/2);
ff=barycentricInterpolation(x,y,xx,c);
end