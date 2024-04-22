%test - false position
ff=@(x) cos(x).*cosh(x)-1;
a=3/2*pi; b=2*pi;
[x,n]=falseposition(ff,a,b,eps)