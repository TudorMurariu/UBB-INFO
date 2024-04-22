function val = evaluate_fundamental_polynomial(nodes, k, x)
  [~, m] = size(nodes);
  u = 1;
  d = 1;
  for j = 1 : m
    if (j ~= k)
      % x - x_j
      u  = u * (x - nodes(1, j));
      % x_k - x_j
      d = d * (nodes(1, k) - nodes(1, j));
    endif
  endfor
  
  val = u / d;
  
endfunction