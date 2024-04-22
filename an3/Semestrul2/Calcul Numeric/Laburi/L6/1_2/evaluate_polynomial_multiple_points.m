function val = evaluate_polynomial_multiple_points(nodes, k, x)
  val = zeros(size(x));
  [~, c] = size(x);
  for i = 1 : c
    val(1, i) = evaluate_fundamental_polynomial(nodes, k, x(1, i));
  endfor
endfunction