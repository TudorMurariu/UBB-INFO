%test resonance
ff=@(x) cos(x).*cosh(x)-1;
ffd=@(x) -sin(x)*cosh(x)+cos(x)*sinh(x);
a=3/2*pi; b=2*pi;
[x0,n0]=bisection(ff,a,b,eps)
[x1,n1]=falseposition(ff,a,b,eps)
[x2,n2]=secant(ff,a,b,0,eps)
[x3,n3]=Newtons(ff,ffd,a,0,eps)