function vi=midpoint(f, a, b, n)
%MIDPOINT - aproximates an integral by midpoint rule
%call VI = MIDPOINT(F, A, B, N)
%F - integrand
%A,B - endpoints of interval
%N - number of subintervals

h = (b-a)/n;
x= a+([0:n-1]+1/2)*h;
vi = h*sum(f(x));