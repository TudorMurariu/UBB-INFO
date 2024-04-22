a = 2;
b = 2;
n = 10;

% Cuadratura Gauss-Jacobi

f = @(x) cos(x) ./ (2 + cos(x));
c = 1 / 2 ^ ((a + b) / 2);
g = @(x) f(acos(x)) ./ sqrt(1 - x .^ 2);
[g_nodes, g_coeff] = Gauss_Jacobi(n, a/2, b/2);
res = g_coeff * g(g_nodes) * c