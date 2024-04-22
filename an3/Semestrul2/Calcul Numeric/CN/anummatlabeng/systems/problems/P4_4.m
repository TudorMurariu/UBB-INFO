%P4_4;
n=input('n=');
A=2*eye(n)-diag(ones(n-1,1),1)-diag(ones(n-1,1),-1);
b=[1:n]';
As=sparse(A);
b=[1:n]';
disp('\ dens')
tic
x1=A\b;
toc
disp('\ rar')
tic
x2=As\b;
toc
disp('lu')
tic
[L,U,P]=lu(A);
x3=U\(L\(P*b));
toc
disp('EG tridiagonal')
a=diag(A,-1); b2=diag(A);
c=diag(A,1);
tic
x4=EGtrid(a,b2,c,b);
toc
fprintf('cond_1(A)=%f\n',condest(A))
