function [A, B] = generare_matrici_herm(n)
    % genereaza sisteme de dimensiune n
    % A - matrice de dimensiune n x n
    % B - matrice coloana de dimensiune n
    A= randi(20, n, n);
    A=triu(A);
    A=A+A'
    B=A*ones(n,1)
end