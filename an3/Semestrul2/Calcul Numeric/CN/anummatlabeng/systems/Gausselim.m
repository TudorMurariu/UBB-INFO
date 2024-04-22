function x=Gausselim(A,b)
%GAUSSELIM - Gaussian elimination with scaled colum pivoting
%call x=Gausselim(A,b)
%A - matrix, b- right hand side vector
%x - solution

[l,n]=size(A);
x=zeros(size(b));
s=sum(abs(A),2);
A=[A,b]; %extended matrix
piv=1:n;
%Elimination
for i=1:n-1
    [u,p]=max(abs(A(i:n,i))./s(i:n)); %pivoting
    p=p+i-1;
    if u==0, error('no unique solution'), end
    if p~=i %line interchange
        piv([i,p])=piv([p,i]);
    end
    for j=i+1:n
        m=A(piv(j),i)/A(piv(i),i);
        A(piv(j),i+1:n+1)=A(piv(j),i+1:n+1)-m*A(piv(i),i+1:n+1);
    end
end
%back substitution
x(n)=A(piv(n),n+1)/A(piv(n),n);
for i=n-1:-1:1
    x(i)=(A(piv(i),n+1)-A(piv(i),i+1:n)*x(i+1:n))/A(piv(i),i);
end