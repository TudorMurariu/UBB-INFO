function P5_4
%P5_4
n=11;
f1=@(x) exp(x);
f2=@(x) x.^(5/2);
f2d=@(x) 5/2*x.^(5/2-1);
f2dd=@(x) 5/2*3/2*x.^(5/2-2);
x=[0:10]/(n-1);
t=linspace(0,1,100);
clf
exgraf(x,f1,f1,f1,t,'exp(x)')
pause
clf
exgraf(x,f2,f2d,f2dd,t,'x^{5/2}')
pause
x=x.^2;
clf
exgraf(x,f2,f2d,f2dd,t,'x^{5/2}')
function exgraf(x,f,fd,fdd,t,tit)
f1=f(x); vt=f(t);
fd1=fd([min(x),max(x)]);
fd2=fdd([min(x),max(x)]);
[a1,b1,c1,d1]=Splinecubic(x,f1,0,fd1);
[a2,b2,c2,d2]=Splinecubic(x,f1,1,fd2);
[a3,b3,c3,d3]=Splinecubic(x,f1,2,[0,0]);
[a4,b4,c4,d4]=Splinecubic(x,f1,3,[0,0]);
z1=valspline(x,a1,b1,c1,d1,t);
z2=valspline(x,a2,b2,c2,d2,t);
z3=valspline(x,a3,b3,c3,d3,t);
z4=valspline(x,a4,b4,c4,d4,t);
plot(t,vt,t,z1,t,z2,t,z3,t,z4);
legend('functia','spline complet','spline cu deriv. secunde',...
    'spline natural', 'spline de Boor',0)
title(strcat('interpolanti spline pentru functia:',tit))
