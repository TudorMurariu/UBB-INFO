function P7_2
f=@(x) 4./(1+x.^2);
format long
I1=Romberg(f,0,1,1e-8,100)
I2=adquad(f,0,1,1e-8)
pi