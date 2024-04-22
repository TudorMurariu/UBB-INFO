%P4_9
N=50; d=ones(N-1,1);
A=2*eye(N)-diag(d,1)-diag(d,-1);
M=tril(A);
N=M-A;
T=M\N;
max(abs(eig(T)))

