%P4_6 problema 4.6
n=input('n=')
A=gensparseband(n,4,3);
b=A*ones(n,1);
[x,ni]=Jacobi(A,b,zeros(n,1),1e-8,300)
[x,ni]=relax(A,b,1,zeros(n,1),1e-8)