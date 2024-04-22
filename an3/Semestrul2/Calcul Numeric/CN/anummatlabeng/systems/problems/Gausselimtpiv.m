function x=Gausselimtpiv(A,b)
%GAUSSELIM - eliminare gaussiana cu pivotare totala
%apel x=Gausselimtpiv(A,b)
%A -matricea, b- vectorul termenilor liberi

[l,n]=size(A);
x=zeros(size(b));
A=[A,b]; %matricea extinsa
pivl=1:n; pivc=1:n;
%Eliminare
for i=1:n-1
    [p,q]=findpiv(abs(A(pivl(i:n),pivc(i:n)))); %pivotare
    p=p+i-1; q=q+i-1;
    if p~=i %interschimbare linii
        pivl([i,p])=pivl([p,i]);
    end
    if q~=i %interschimbare linii
        pivc([i,q])=pivc([q,i]);
    end

    for j=i+1:n
        m=A(pivl(j),pivc(i))/A(pivl(i),pivc(i));
        A(pivl(j),[pivc(i+1:n),n+1])=A(pivl(j),[pivc(i+1:n),n+1])-...
            m*A(pivl(i),[pivc(i+1:n),n+1]);
    end
end
%substitutie inversa
x(n)=A(pivl(n),n+1)/A(pivl(n),pivc(n));
for i=n-1:-1:1
    x(i)=(A(pivl(i),n+1)-A(pivl(i),pivc(i+1:n))*...
        x(i+1:n))/A(pivl(i),pivc(i));
end
[pp,jj]=sort(pivc);
x=x(jj);
function [i,j]=findpiv(A)
u=max(A(:));
[ii,jj]=find(A==u);
i=ii(1); j=jj(1);