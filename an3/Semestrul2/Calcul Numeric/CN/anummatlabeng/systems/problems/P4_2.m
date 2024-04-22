%P4_2
n=input('n=');
A=rand(n);
b=A*ones(n,1);
disp('EG cu permutare logica')
tic
x=Gausselim(A,b);
toc
disp('EG cu permutare fizica')
tic
x=Gausselim2(A,b);
toc
disp('LUP cu permutare logica')
tic
[L,U,P]=lup2(A);
toc
disp('LUP cu permutare fizica')
tic
[L,U,P]=lup(A);
toc