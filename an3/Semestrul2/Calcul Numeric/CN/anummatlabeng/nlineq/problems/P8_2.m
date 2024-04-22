function P8_2(a)
f2=@(x) sign(x-a).*sqrt(abs(x-a));
f2d=@(x) 1./(2*sqrt(abs(x-a)));
[z,ni]=secant(f2,-a-1,a+1,1e-6,0,150)
x=Newtonsmod(f2,f2d,a+1,1e-6,0,199);
reshape(x,8,25)'