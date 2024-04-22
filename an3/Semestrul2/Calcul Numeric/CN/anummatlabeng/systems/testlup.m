%testlup
A=rand(n);
disp('cu interschimbare efectiva')
tic
[L,U,P]=lup(A);
toc
disp('cu interschimbare logica')
tic
[L1,U1,P1]=lup2(A);
toc