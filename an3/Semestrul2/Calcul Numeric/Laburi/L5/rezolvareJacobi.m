function x = rezolvareJacobi(A, b, err)
% rezolvare Jacobi
% A - matricea sistemului
% b - vectorul termenilor liberi
% err - eroarea de calculare a solutiei
    M = diag(diag(A));
    N = M - A;
    invM = inv(M);
    T = invM * N;
    c = invM * b;
    alfa = norm(T, inf);
    
    x0 = zeros(size(b));
    x=x0(:);

    solFound = 0;
    while solFound == 0
      x0 = x;
      x = T*x0+c;
      if norm(x-x0, inf)<(1-alfa) / alfa*err
        solFound = 1;
      end
    end
end
