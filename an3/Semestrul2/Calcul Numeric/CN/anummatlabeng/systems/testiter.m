%testiter
o1=ones(n-1,1);
A=5*eye(n)-diag(o1,1)-diag(o1,-1);
b=A*ones(n,1);
err=eps;
%determin parametrul optim al realaxarii
w=relopt(A);
tic
%Jacobi
[x1,ni1]=Jacobi(A,b,b,err,1000); 
%Gauss-Seidel
[x2,ni2]=relax(A,b,1,b,err,1000); 
%relaxare
[x3,ni3]=relax(A,b,w,b,err,1000); 
toc
ni1,ni2,ni3