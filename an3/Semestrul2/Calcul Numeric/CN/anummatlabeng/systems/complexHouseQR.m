function [R,Q]=complexHouseQR(A)
%HouseQR - QR decomposition with Househoulder reflexion
%in complex
%call [R,Q]=complexHouseQR(A)
%A mxn matrix, R upper triangular superior, Q unitary

[m,n]=size(A);
v=zeros(m,n); %reflection vectors
%cumpute R
for k=1:n
    x=A(k:m,k);
    a=angle(x(1));
    f=exp(i*a)*norm(x)*[1;zeros(length(x)-1,1)];
    v1=x+f; v2=x-f;
    if(norm(v1)>=norm(v2))
        v(k:m,k)=v1/norm(v1);
    else
        v(k:m,k)=v2/norm(v2);
    end
    A(k:m,k:n)=A(k:m,k:n)-2*v(k:m,k)*(v(k:m,k)'*A(k:m,k:n));
end
R=triu(A);
if nargout==2 %Q wanted
    Q=eye(m,n);
    for j=1:n
        for k=n:-1:1
            Q(k:m,j)=Q(k:m,j)-2*v(k:m,k)*(v(k:m,k)'*Q(k:m,j));
        end
    end
end

