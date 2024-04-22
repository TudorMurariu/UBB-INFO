function [A, b] = generare_matrici_rare(n, density, min_val, max_val)
% Generate a diagonally dominant sparse matrix with random values
% m: number of rows
% n: number of columns
% density: sparsity level (between 0 and 1)
% min_val: minimum value of the elements
% max_val: maximum value of the elements
% A: the sparse matrix

% Generate a random sparse matrix with the specified sparsity level
A = sprand(n, n, density);

% Ensure that the matrix is diagonally dominant
diag_vals = abs(sum(A, 2)) + eps; % add a small value to avoid dividing by zero
A = spdiags(diag_vals, 0, n, n) - A;

% Scale the random values to the desired range
A = A.*(max_val - min_val) + min_val;

% Convert the matrix to full format
A = full(A);
b = A\([1:n]');
end