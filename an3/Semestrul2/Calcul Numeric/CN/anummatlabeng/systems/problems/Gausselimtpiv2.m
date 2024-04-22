function x=Gausselimtpiv2(A,b)
%GAUSSELIMTPIV2 - eliminare gaussiana cu pivotare totala
%apel x=Gausselim2(A,b)
%A -matricea, b- vectorul termenilor liberi

[l,n]=size(A);
x=zeros(size(b));
A=[A,b]; %matricea extinsa
pivc=1:n;
%Eliminare
for i=1:n-1
    [p,q]=findpiv(abs(A(i:n,i:n))); %pivotare
    p=p+i-1; q=q+i-1;
    if p~=i %interschimbare linii
        %A([i,p],i:n+1)=A([p,i],i:n+1); 
        A([i,p],:)=A([p,i],:); 
    end
    if q~=i %interschimbare coloane
        pivc([i,q])=pivc([q,i]);
        %A(i:n,[i,q])=A(i:n,[q,i]);
        A(:,[i,q])=A(:,[q,i]);
    end

    for j=i+1:n
        m=A(j,i)/A(i,i);
        A(j,i+1:n+1)=A(j,i+1:n+1)-m*A(i,i+1:n+1);
    end
end
%substitutie inversa
x(n)=A(n,n+1)/A(n,n);
for i=n-1:-1:1
    x(i)=(A(i,n+1)-A(i,i+1:n)*x(i+1:n))/A(i,i);
end
[pp,jj]=sort(pivc);
x=x(jj);
function [i,j]=findpiv(A)
u=max(A(:));
[ii,jj]=find(A==u);
i=ii(1); j=jj(1);