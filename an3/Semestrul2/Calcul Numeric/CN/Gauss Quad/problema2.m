% u = t/2; t = 2u; dt = 2*du

f = @(u) 2*cos(2*u);
epsilon = 1e-10;
n = 2;

[g_nodes,g_coeff] = Gauss_Cheb1(n);
integral_n = sum(g_coeff .* arrayfun(f, g_nodes)');

while 1
  n = n + 1;
  [g_nodes,g_coeff] = Gauss_Cheb1(n);
  integral_nplus1 = sum(g_coeff .* arrayfun(f, g_nodes)');
  if abs(integral_nplus1 - integral_n) < epsilon
    break
  endif
  integral_n = integral_nplus1;
endwhile

disp('Integrala functiei date este aproximativ: ');
disp(integral_nplus1);