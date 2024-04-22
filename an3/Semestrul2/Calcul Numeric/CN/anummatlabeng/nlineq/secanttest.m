%test - secant method
ff=@(x) cos(x).*cosh(x)-1;
a=3/2*pi; b=2*pi;
[x,n]=secant(ff,a,b,0,eps)