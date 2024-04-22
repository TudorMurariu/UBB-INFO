%tester
o1=ones(n-1,1);
o2=ones(n-2,1);
A=5*eye(n)-diag(o1,1)-diag(o1,-1)-diag(o2,2);%-diag(o2,-2);
As=sparse(A);
b=A*ones(n,1);
w=relopt(A);
err=eps;
tic
[z1,x1,n1]=Jacobit(As,b,b,err,1000);
[z2,x2,n2]=relaxt(As,b,1,b,err,1000);
[z3,x3,n3]=relaxt(As,b,w,b,err,1000);
toc
clear r*
r1=kron(ones(1,n1+1),b)-As*x1(:,1:n1+1);
r2=kron(ones(1,n2+1),b)-As*x2(:,1:n2+1);
r3=kron(ones(1,n3+1),b)-As*x3(:,1:n3+1);
for k=1:n1+1
    rr1(k)=norm(r1(:,k),inf)/norm(x1(:,k),inf);
end
for k=1:n2+1
    rr2(k)=norm(r2(:,k),inf)/norm(x2(:,k),inf);
end
for k=1:n3+1
    rr3(k)=norm(r3(:,k),inf)/norm(x3(:,k),inf);
end
semilogy([1:n1+1],rr1,'k-+',[1:n2+1],rr2,'k-x',[1:n3+1],rr3,'k-d')
legend(strcat('Jacobi - ',int2str(n1), ' iteratii'),...
    strcat('Gauss-Seidel - ',int2str(n2), ' iteratii'),...
    strcat('SOR - ',int2str(n3), ' iteratii'))