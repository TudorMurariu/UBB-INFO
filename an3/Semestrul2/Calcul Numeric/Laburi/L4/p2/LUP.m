function [L,U,P]=LUP(A)
% descompunerea LUP a matricei A
% A - matricea de descompus
% L - matricea multiplicarilor
% U - matricea finala
% P - matricea de permutare

[m,n] = size(A);
P = zeros(m,n);
pivot = (1:m)';

for i=1:m-1
    [maxim, ppoz] = max(abs(A(i:m,i)));
    ppoz = ppoz+i-1;

    if i~=ppoz
        A([i, ppoz],:) = A([ppoz,i],:);
        pivot([i,ppoz]) = pivot([ppoz,i]);
    end

    % determinare complement Schur
    row = i+1:m;
    A(row,i) = A(row,i)/A(i,i);
    A(row,row) = A(row,row)-A(row,i)*A(i,row);
end

for i=1:m
    P(i,pivot(i)) = 1;
end

U = triu(A); % triunghiul de sus al matricei
L = tril(A,-1); % triunghiul de jos al matricei (fara diagonala principala)
L = L+eye(m); % setam 1 pe diagonala principala
end
