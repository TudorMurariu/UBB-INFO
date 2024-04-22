%P7_1b - nu se evita impartirea la zero
f1=@(x) sin(x)./x;
%[I1,fc1]=adquad(f1,0,1)
[I2,fc2]=quad(f1,0,1)
[I3,fc3]=quadl(f1,0,1)

