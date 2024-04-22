function R = RombergMat(f, a, b, n = 3, m = n)
  % ROMBERGMAT - calcul tabelului pentru metoda lui Romberg
  % call R = RombergMat(f, a, b, n)
  % f - functia
  % a, b - capetele intevalului
  % n - numarul de iteratii, default = 3
  % m - numarul de coloane ce sa fie completate, default = n

  R = zeros(n, m);
  h = b - a;

  % prima iteratie
  R(1, 1) = h / 2 * (sum(f([a, b])));

  for k = 2: n
     % formula trapezului
     x = a + ([1: 2 ^ (k - 2)] - 0.5) * h;
     R(k, 1) = 0.5 * (R(k - 1, 1) + h * sum(f(x)));

     % extrapolare
     plj = 4;
     for j = 2: m
        R(k, j) = (plj * R(k, j - 1) - R(k - 1, j - 1)) / (plj - 1);
        plj = plj * 4;
     endfor

     %halving step
     h = h / 2;
  endfor
endfunction
