% u = (t - 3/2)/(1/2); t = u/2 + 3/2; dt = du/2

f = @(u) 2.*sqrt(3.*(u./2 + 3/2) - (u./2 + 3/2).^2 - 2) .* sin(u./2 + 3/2);

eps = 1e-10;
n = 1;

[g_nodes,g_coeff] = Gauss_Cheb2(n);
integral_n = sum(g_coeff .* arrayfun(f, g_nodes)');

while 1
  n = n + 1;
  [g_nodes,g_coeff] = Gauss_Cheb2(n);
  integral = sum(g_coeff .* arrayfun(f, g_nodes)');
  if abs(integral - integral_n) < eps
    break
  endif
  integral_n = integral;
endwhile

disp('Integrala functiei date este aproximativ: ');
disp(integral);