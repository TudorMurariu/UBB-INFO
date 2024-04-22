%P5_1
%exemplu B-spline liniar mcmmp
f=@(x) sin(pi*(x-1)/3);
x=1:6;
t=1:0.05:6;
c=mcmmpslin(f,x);
y=valbsplinel(c,t,x);
plot(t,y,t,f(t));