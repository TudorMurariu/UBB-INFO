function [L,U,P,Q]=luptpiv(A)
%LUP2 descompunerea LUP a matricei A cu pivotare totala
%apel [L,U,P,Q]=lup(A)
%interschimbarea logica a liniilor

[m,n]=size(A);
P=eye(m,n); Q=eye(m,n);
for i=1:m-1
    %pivotare
    [p,q]=findpiv(abs(A(i:n,i:n)));
    p=p+i-1; q=q+i-1;
    %interschimbare linii
    if p~=i
        P([i,p],:)=P([p,i],:);
        A([i,p],:)=A([p,i],:);
    end
    %interschimbare coloane
    if q~=i
        Q(:,[i,q])=Q(:,[q,i]);
        A(:,[i,q])=A(:,[q,i]);
    end

    %complement Schur
    lin=i+1:m;
    A(lin,i)=A(lin,i)/A(i,i);
    A(lin,lin)=A(lin,lin)-A(lin,i)*A(i,lin);
end;

U=triu(A);
L=tril(A,-1)+eye(m);


function [i,j]=findpiv(A)
u=max(A(:));
[ii,jj]=find(A==u);
i=ii(1); j=jj(1);
