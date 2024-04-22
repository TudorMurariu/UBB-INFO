function [L,U,P]=LUPtrid(A)
%LUPTRID - descompunere LUP pentru matrice tridiagonala

[m,n]=size(A);
P=zeros(m,n);
piv=[1:m]';
for i=1:n-1
    %pivotare
    if abs(A(i,i))<abs(A(i+1,i))
        piv([i,i+1])=piv([i+1,i]);
        A([i,i+1],:)=A([i+1,i],:);
    end
    A(i+1,i)=A(i+1,i)/A(i,i);
    A(i+1,i+1:min(i+2,n))=A(i+1,i+1:min(i+2,n))-...
        A(i+1,i)*A(i,i+1:min(i+2,n));
end
U=triu(A,0);
L=spdiags(ones(n,1),0,n,n)+tril(A,-1);
for i=1:m
    P(i,piv(i))=1;
end