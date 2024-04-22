function x = rezolvareSOR(A, b, omega, err)
% metoda relaxarii (Successive OverRelaxation)
% A - matricea sistemului
% b - vectorul termenilor liberi
% omega - parametrul relaxarii
% err - eroarea de calculare a solutiei
% x - solutia
  [n, ~]=size(A);
  x0=zeros(size(b));
  
  M = 1 / omega*diag(diag(A))+tril(A,-1);
  N = M-A;
  T = M\N;
  c = M\b;
  alfa = norm(T, inf);
  x = x0(:);
  solFound = 1;
  while solFound == 1
    x0 = x;
    x = T*x0 + c;
    if norm(x-x0, inf) < (1-alfa) / alfa*err
        solFound = 0;
    end
  end
end