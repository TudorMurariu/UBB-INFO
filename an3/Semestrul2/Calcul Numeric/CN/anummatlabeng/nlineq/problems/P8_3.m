%P8_3
f=@(x) x.^3-2*x-5;
fd=@(x) 3*x.^2-2;
p=[1,0,-2,-5];
u=solve('x^3-2*x-5=0')
rs=double(u)
r=roots(p)
xr1=fzero(f,3)
xr2=Newtons(f,fd,3,1e-5)
xc1=Newtons(f,fd,-1+2i,1e-5,0,200)
xc2=Newtons(f,fd,-1-2i,1e-5,0,200)