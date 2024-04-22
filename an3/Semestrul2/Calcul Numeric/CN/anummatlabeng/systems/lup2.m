function [L,U,P]=lup2(A)
%LUP2 determina descompunerea LUP a matricei A
%apel [L,U,P]=lup(A)
%interschimbarea logica a liniilor

[m,n]=size(A);
P=zeros(m,n);
piv=1:m;
for i=1:m-1
    %pivotare
    [pm,kp]=max(abs(A(piv(i:m),i)));
    kp=kp+i-1;
    l=piv(i);
    %interschimbare linii
    if i~=kp
        piv([i,kp])=piv([kp,i]);
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

