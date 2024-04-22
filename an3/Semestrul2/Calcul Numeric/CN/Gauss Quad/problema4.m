% Definirea func?iei f(u) dup? schimbarea de variabil?
f = @(u) ( u - 1/2) .^2 .* cos(u - 1/2);


eps = 1e-7;
n = 1;

[g_nodes,g_coeff] = Gauss_Hermite(n);
integral_n = sum(g_coeff .* arrayfun(f, g_nodes)');

while 1
  n = n + 1;
  [g_nodes,g_coeff] = Gauss_Hermite(n);
  integral = sum(g_coeff .* arrayfun(f, g_nodes)');
  if abs(integral - integral_n) < eps
    break
  endif
  integral_n = integral;
endwhile

disp('Integrala functiei date este aproximativ: ');
disp(e^(1/4) .* integral);
