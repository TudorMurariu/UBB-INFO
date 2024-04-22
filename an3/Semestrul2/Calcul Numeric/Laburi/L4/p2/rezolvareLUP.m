function x = rezolvareLUP(A, b)
% rezolvarea sistemului pe baza descompunerii LUP
% A - matricea sistemului 
% b - vectorul termenilor liberi
% x - rezultatul sistemului

[L,U,P] = LUP(A);
y = L\(P*b);
x = U\y; 
end