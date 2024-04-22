function x = rezolvareCholesky(A, b)
% determinarea solutiei sistemului pe baza descompunerii Cholesky
% A - matricea sistemului 
% b - vectorul termenilor liberi
% x - rezultatul sistemului

R = descompunereCholesky(A);
y = (R.')\b;
x = R\y; 
end