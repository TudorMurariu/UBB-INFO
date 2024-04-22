function [A, b] = generareSistem1(n)
% genereaza primul sistem
% n - dimensiunea sistemului
% A - matricea sistemului
% b - matricea termenilor liberi
  A = full(spdiags([-ones(n,1), 5*ones(n,1), -ones(n,1)], -1:1, n, n));
  b = A * ones(n,1);
end