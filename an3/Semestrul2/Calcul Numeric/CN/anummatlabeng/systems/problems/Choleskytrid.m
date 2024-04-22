function R=Choleskytrid(A)
%CHOLESKY - factorizare Cholesky a unei matrice tridiagonale
%apel R=Cholesky(A)
%A - matrice hermitiana tridiagonala
%R - matrice triunghiulara superior

[m,n]=size(A);
for k=1:m
    if A(k,k)<=0
        error('matricea nu este hpd')
    end
    if k<m
        A(k+1,k+1)=A(k+1,k+1)-A(k,k+1)*A(k,k+1)/A(k,k);
    end
    A(k,k:min(k+1,m))=A(k,k:min(k+1,m))/sqrt(A(k,k));
end
R=triu(A);