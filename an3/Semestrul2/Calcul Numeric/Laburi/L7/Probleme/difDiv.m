function [z, td] = difDiv(x, y, dy)
% x - nodurile
% y - valoarea functiei in noduri
% dy - valoarea functiei derivate in noduri

  m = length(x);
  z = zeros(1,2*m);
  lz = length(z);
  td = zeros(lz,lz);

  for i = 1:m
    z(2*i-1) = x(i);
    z(2*i) = x(i);
    td(2*i - 1, 1) = y(i);
    td(2*i, 1) = y(i);
    td(2*i, 2) = dy(i);
    if(i > 1)
      td(2*i - 1, 2) = (td(2*i - 1, 1) - td(2*i -2, 1)) / (z(2*i - 1) - z(2*i - 2));
    end
  end

  for i =3:2*m
      for j = 3:i
          td(i, j) = (td(i, j-1) - td(i - 1, j - 1))/(z(i) - z(i -j + 1)); 
      end
  end
  td = diag(td)';
end