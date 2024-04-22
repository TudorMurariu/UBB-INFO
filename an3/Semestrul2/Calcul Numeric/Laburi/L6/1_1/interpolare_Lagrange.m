function val = interpolare_Lagrange(nodes, nodevals, point)
  val = 0;
  
  [~, cols] = size(nodes);
  m = cols;
  x = point;
  
  for k = 1 : m
    % COmpute f(x_k)
    f = nodevals(1, k);
    u = 1;
    d = 1;
    for j = 1 : m
      if (j ~= k)
        % Compute x - xj
        u = u * (x - nodes(1, j));
        % Compute xk - xj 
        d = d * (nodes(1, k) - nodes(1, j));
      endif
    endfor
    
    val = val + f * (u/d);
    
  endfor
  
endfunction
