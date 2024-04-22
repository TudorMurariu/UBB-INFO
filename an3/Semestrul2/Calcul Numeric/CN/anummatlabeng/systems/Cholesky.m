function R=Cholesky(A)
%CHOLESKY - Cholesky factorization
%call R=Cholesky(A)
%A - HPD matrix 
%R - upper triangular matrix

[m,n]=size(A);
for k=1:m
    if A(k,k)<=0
        error('matrix is not HPD')
    end
    for j=k+1:m
        A(j,j:m)=A(j,j:m)-A(k,j:m)*A(k,j)/A(k,k);
    end
    A(k,k:m)=A(k,k:m)/sqrt(A(k,k));
end
R=triu(A);