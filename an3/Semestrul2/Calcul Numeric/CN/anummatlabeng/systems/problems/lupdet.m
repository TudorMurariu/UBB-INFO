function [L,U,P,D]=lupdet(A)
%determina descompunerea LUP a matricei A si determinatul
%apel [L,U,P,D]=lupdet(A)
%interschimbarea logica a liniilor

[m,n]=size(A);
P=zeros(m,n);
piv=1:m;
s=1;
for i=1:m-1
    %pivotare
    [pm,kp]=max(abs(A(piv(i:m),i)));
    kp=kp+i-1;
    l=piv(i);
    %interschimbare linii
    if i~=kp
        piv([i,kp])=piv([kp,i]);
        s=-s;
    end
    %complement Schur
    lin=i+1:m;
    A(piv(lin),i)=A(piv(lin),i)/A(piv(i),i);
    A(piv(lin),lin)=A(piv(lin),lin)-A(piv(lin),i)*A(piv(i),lin);
end;
for i=1:m
    P(i,piv(i))=1;
end;
U=triu(A(piv,:));
L=tril(A(piv,:),-1)+eye(m);

D=s*prod(diag(U));