function [R,Q]=HouseholderQR(A)
%HOUSEHOLDERQR calculeaza factorizarea QR a unei matrice reale

[m,n]=size(A);
beta=zeros(n,1);
for j=1:n
    [v,beta(j)]=house(A(j:m,j));
    A(j:m,j:n)=A(j:m,j:n)-beta(j)*v*(v'*A(j:m,j:n));
    if j<m
        A(j+1:m,j)=v(2:m-j+1);
    end
end
R=triu(A(1:n,:));
v=zeros(n,1);
if nargout==2 %se doreste Q
    Q=eye(m,n);
    for j=n:-1:1
        v(j:m)=[1;A(j+1:m,j)];
        Q(j:m,j:n)=Q(j:m,j:n)-beta(j)*v(j:m)*(v(j:m)'*Q(j:m,j:n));
    end
end
    