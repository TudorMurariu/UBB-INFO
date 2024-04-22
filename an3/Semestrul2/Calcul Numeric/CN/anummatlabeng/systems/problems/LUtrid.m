function [L,U]=LUtrid(A)
%LUTRID - descompunere LU pentru matrice tridiagonala

[m,n]=size(A);
for i=1:n-1
    A(i+1,i)=A(i+1,i)/A(i,i);
    A(i+1,i+1)=A(i+1,i+1)-A(i+1,i)*A(i,i+1);
end
U=triu(A,0);
L=spdiags(ones(n,1),0,n,n)+tril(A,-1);