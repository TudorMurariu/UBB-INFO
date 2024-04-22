function [L,U,P] = lup(A)
    % descompunerea LUP a matricei A cu interschimbare fizica
    % A - matricea de descompus
    % L - matricea multiplicarilor
    % U - matricea finala
    % P - pivotarea
    
    [m,n] = size(A);
    P = zeros(m,n);
    piv = (1:m)';  % matrice coloana
    
    for i=1:m-1
        % caut valoarea maxima (+ pozitia) unde sa fac pivotarea
        [maxim,ppoz] = max(abs(A(i:m,i)));
        ppoz = ppoz+i-1; % pozitie pivot
        
        if i ~= ppoz
            %interschimbare linii daca pivotul nu e egal cu linia curenta
            A([i,ppoz],:) = A([ppoz,i],:);
            piv([i,ppoz]) = piv([ppoz,i]);
        end
        % determinare complement Schur
        row = i+1:m;
        A(row,i) = A(row,i) / A(i,i);
        A(row,row) = A(row,row)-A(row,i) * A(i,row);
    end
    for i = 1:m
        P(i,piv(i)) = 1;
    end
    
    U = triu(A); % decupeaza triunghiul de sus al matricei
    L = tril(A,-1); % decupeaza triunghiul de jos al matricei fara diag princ
    L = L + eye(m); % setam 1 pe diagonala principala
end