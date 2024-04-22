function fAprox = pade_aprox(x, c, m, k)
  if length(c) < m+k+2
      error('Sunt necesari cel putin %i coeficienti c', m+k+2)
  end

  C = zeros(k);
  y = zeros(k, 1);
  for i = 1:k
    for j = 1:k
      if m+i-j>0
        C(i, j) = c(m+i-j+1);
      end
    end
    y(i) = -c(m+i+1);
  end
  b = C \ y; % rezolva sistemul C*b=y => gasim coeficientii b

  b = [1; b]; % adaugam 1 pe prima coloana (b(1)=1, prin constructie)

  a = zeros(m+1, 1);
  for j=1:m+1
    for l=1:min(j, k+1) % pentru l>k+1 avem b(l) = 0
      a(j) = a(j) + c(j-l+1)*b(l);
    end
  end

  % evaluam polinoamele in punctul x si le impartim
  fAproxNumarator = polyval(a(m+1:-1:1), x);
  fAproxNumitor = polyval(b(k+1:-1:1), x);
  fAprox = fAproxNumarator./fAproxNumitor;
end
