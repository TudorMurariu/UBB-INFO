function pxi = hermite(x, y, dy, xi)
% x - nodurile
% y - valoarea functiei in noduri
% dy - valoarea derivatei functiei in noduri
% xi - puncte de evaluat
% pxi - aproximarea functiei
  
  [z, Q] = difDiv(x, y, dy);

  lx = length(xi);
  lz = length(z);
    
  for i = 1:lx
      x_diff = xi(i) - z;
      y(i) = [1, cumprod(x_diff(1:lz-1))]*Q';
  end
  [~, nrCol] = size(xi);
  pxi = y(1: nrCol);
end