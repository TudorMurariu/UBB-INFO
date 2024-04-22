function x = rezolvare_cholesky(A, B)
    % determinarea solutiei sistemului pe baza descompunerii Cholesky
    % A - matricea sistemului 
    % B - vectorul termenilor liberi
    % x - rezultatul sistemului
    
    R = cholesky(A);
    y = (R.')\B;
    x = R\y; 
end