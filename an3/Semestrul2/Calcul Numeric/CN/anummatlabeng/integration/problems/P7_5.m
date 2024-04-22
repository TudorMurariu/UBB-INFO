%P7_5
f7_5=@(t) 1./sin(sqrt(abs(t)));
I1=adquadtr(f7_5,-1,2,1e-6,1)

I2=quad(f7_5,-1,2,1e-6,1)