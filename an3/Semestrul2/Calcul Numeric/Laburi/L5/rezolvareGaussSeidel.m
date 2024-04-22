function sol = rezolvareGaussSeidel(A, b, err)
    % metoda Gauss-Seidel
    % A - matricea sistemului
    % b - vectorul termenilor liberi
    % err - eroarea de calculare a solutiei
    % x - solutia sistemului
    
    m = size(A);
    x = zeros(m(1), 1);
    M = tril(A);
    N = M - A;
    T = M\N;
    c = M\b;
    alfa = norm(T, inf);
    solFound = 0;
    i = 1;
    while solFound == 0
       x(:, i+1) = T*x(:, i) + c;
       if norm(x(:,i+1)-x(:,i), inf) < (1-alfa)/alfa*err
          sol = x(:,i+1);
          solFound = 1;
       end
       i = i+1;
    end
end