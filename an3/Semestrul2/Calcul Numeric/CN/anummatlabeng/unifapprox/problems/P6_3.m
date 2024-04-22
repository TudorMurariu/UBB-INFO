%P6_3
ff=@(x) x./(x+1);
k=input('gradul:'); n=input('n=');
kn=noduri(n,k);
%kn=kn/max(kn);
aa=min(kn); bb=max(kn); %limitele

t=linspace(aa,bb,200);
[c,v]=Bspline_mcmmp(k,n,kn,ff,t);
plot(t,ff(t),t,v)
s=sprintf('Aproximanta B-spline, gradul=%d, n=%d',k,n);
legend('functia','aproximanta',0)
title(s)