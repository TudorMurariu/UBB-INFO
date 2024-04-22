function x = descompunere_lup(A, B)
    % descompunerea solutiei sistemului pe baza descompunerii LUP
    % A - matricea sistemului 
    % B - vectorul termenilor liberi
    % x - rezultatul sistemului
    
    [L, U, P] = lup(A);
    det(L)
    U
    y = L \ (P * B);
    x = U \ y; 
end