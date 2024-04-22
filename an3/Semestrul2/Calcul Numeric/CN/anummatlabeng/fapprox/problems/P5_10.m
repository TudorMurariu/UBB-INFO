%P5_10 - test spline periodic; noduri alese aleator
ff=@(x) sin(x.^2).*sin(x);
x=2*pi*rand(1,10);
x=sort(x); x=[0,x,2*pi];
y=ff(x); t=linspace(0,2*pi,200);
[a,b,c,d]=Splineperiodic(x,y);
z=valspline(x,a,b,c,d,t);
plot(x,ff(x),'o',t,ff(t),t,z)
legend('functia','spline-ul periodic',0)

