function [Q,R]=GivensQR(A)
%GIVENSQR - QR decomposition with Givens rotations

[m,n]=size(A);
Q=eye(m,m);
for j=1:n
    for i=m:-1:j+1
        [c,s]=Givens(A(i-1,j),A(i,j));
        A(i-1:i,j:n)=[c,-s; s,c]*A(i-1:i,j:n);
        A(i,j)=encode(c,s);
        Q(:,i-1:i) = Q(:,i-1:i)*[c -s; s c]';
    end
end
R=triu(A);
