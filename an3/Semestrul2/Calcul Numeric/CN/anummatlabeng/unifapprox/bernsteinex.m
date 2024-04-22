%reprezentare polinoame Bernstein
clf
t=linspace(0,1,100);
y=sin(2*pi*t);
plot(t,y,'k-'); hold on
for m=[10,15,100]
    x=linspace(0,1,m);
    yc=sin(2*pi*x);
    yb=deCasteljau(yc,t);
    plot(t,yb,'k--');
end