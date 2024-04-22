function lissajous(a,b,c,n)
t=linspace(0,2*pi,250);
x=a*sin(n*t+c);
y=b*sin(t);
plot(x,y)