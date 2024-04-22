function [A, b] = generareSistemDiagDomiante(n)
    A = rand(n);
    A = A + eye(n) * n;
    sol = [1: n]';
    b = A * sol;
end
