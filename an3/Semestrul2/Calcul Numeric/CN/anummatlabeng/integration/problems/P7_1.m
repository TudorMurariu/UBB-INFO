function P7_1
%P7_1 - se evita impartirea la zero

[I2,fc2]=adquad(@f2,0,1)
I=Romberg(@f2,0,1,1e-6,50)
[I3,fc3]=quad(@f2,0,1)
[I4,fc4]=quadl(@f2,0,1)

function y=f2(x)
r1=find(abs(x)<eps);
r2=find(abs(x)>=eps);
y(r1)=ones(size(x(r1)));
y(r2)=sin(x(r2))./x(r2);
