%P4_8
f=@(x) x.*(x-1);
N=20;
[x,v]=Poisson1D(f,N);

[x2,v2]=Poisson1DSOR(f,N);
plot(x,v,x2,v2)