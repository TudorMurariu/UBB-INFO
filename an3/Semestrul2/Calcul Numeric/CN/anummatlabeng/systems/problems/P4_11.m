%P4_11
%syms A b x
A=[1,1;10,10^18];
b=[2;10+10^18];
x1=Gausselim(A,b)
x2=A\b

A2=[1,1;1e-17,1];
b2=[2;1+1e-17];
x3=Gausselim(A2,b2)
x4=A2\b2

z=solve('x+y=2','10*x+10^18*y=10+10^18');
z.x,z.y