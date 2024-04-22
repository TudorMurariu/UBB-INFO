function P5_3
%P5_3
n=11;
f1=@(x) exp(x);
f2=@(x) x.^(5/2);
f2d=@(x) 5/2*x.^(5/2-1);
x=[0:10]/(n-1);
t=linspace(0,1,100);
clf
exgraf(x,f1,f1,t,'exp(x)')
pause
clf
exgraf(x,f2,f2d,t,'x^{5/2}')
pause
x=x.^2;
clf
exgraf(x,f2,f2d,t,'x^{5/2}')
function exgraf(x,f,fd,t,tit)
f1=f(x); vt=f(t);
fd1=fd(x);
y1=lagr(x,f1,t);
[z,td]=difdivnd(x,f1,fd1);
y2=pNewton(td,z,t);
plot(t,vt,t,y1,t,y2);
legend(tit,'Lagrange','Hermite',0)
title(strcat('exemple pentru:  ', tit))
